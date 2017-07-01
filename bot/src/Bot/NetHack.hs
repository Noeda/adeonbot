{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Bot.NetHack
  ( runAdeonbot )
  where

import Bot.NetHack.AIEntry
import Bot.NetHack.Config
import Bot.NetHack.Logs
import Bot.NetHack.Synchronizer
import Bot.NetHack.WebDiagnostics
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.ST
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Data
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.Time
import Data.Yaml
import GHC.Generics
import System.Clock
import System.Environment
import System.Exit
import System.IO
import Terminal.Emulator
import Terminal.Pty
import Terminal.Screen
import Terminal.Terminal

-- | Thrown when oscillation detection kicks in
data BotOscillated = BotOscillated
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

instance Exception BotOscillated

runAdeonbot :: IO ()
runAdeonbot = do
  args <- getArgs
  case args of
    [] -> showUsage stdout >> exitSuccess
    [config_file] -> do
      f <- B.readFile config_file
      case decodeEither f of
        Left exc -> do
          hPutStrLn stderr $ "Reading config file failed: " <> show exc
          exitFailure
        Right config -> run config
    _ -> do
      hPutStrLn stderr "Too many arguments."
      showUsage stderr
      exitFailure

showUsage :: Handle -> IO ()
showUsage handle = do
  hPutStrLn handle "Usage:"
  hPutStrLn handle "adeonbot [CONFIG FILE]"

botEntry :: BotConfig -> TVar (ActivityReport, Maybe B.ByteString) -> (WorldState -> STM ()) -> IO ()
botEntry config activity_report_tvar update_world = do
  let bot_code = makeBotLogicRunner bot_runner
      sender = makeSender send

      activity_syncer = makeActivitySyncer $ fmap fst $ atomically $ readTVar activity_report_tvar

  initial_ai_state <- emptyAIState config

  runSynchronizer bot_code sender activity_syncer (fromIntegral $ latency config) initial_ai_state
 where
  send bs =
    atomically $ do
      (old_ar, sending) <- readTVar activity_report_tvar
      -- Wait if the previous data hasn't been consumed yet from send buffer
      when (isJust sending) retry

      writeTVar activity_report_tvar $ (old_ar { pendingDataSend = True }, Just bs)
    
  bot_runner state screen_state cx cy = do
    -- Oscillation check; has turn count changed in last N seconds?
    now <- getCurrentTime
    oscillates <- doesAILookLikeItsOscillating state
    if oscillates
      then sendBailoutString send >> throwIO BotOscillated
      else do let (new_aistate, sending) = stepAIState now screen_state cx cy state

              -- Update the world state so diagnostics can pick it up
              case getAIStateWorldState new_aistate of
                Nothing -> return ()
                Just world -> atomically $ update_world world

              when (B.null sending) $
                logError "botEntry: AI decided not to send anything to NetHack. Panic."
              return (new_aistate, sending)

sendBailoutString :: (B.ByteString -> IO ()) -> IO ()
sendBailoutString send = do
  send "\x1b \x1b \x1b \x1b\n \n"
  threadDelay 500000
  send "\x1b \x1b \x1b \x1b\n \n"
  threadDelay 500000
  send "\x1b \x1b \x1b \x1b\n \n"
  threadDelay 500000
  send "\x1b \x1b \x1b \x1b\n \n"
  threadDelay 500000
  send "#quit\n"
  threadDelay 500000
  send "y"
  threadDelay 500000
  send "q q q q q q \x1b"
  threadDelay 500000
  send "q   q   q   \x1b"
  threadDelay 500000
  send "            \x1b"

run :: BotConfig -> IO ()
run config = withRawTerminalMode $ do
  let cmd = fmap T.unpack $ nethackCommand config

  tid <- myThreadId

  withProcessInPty (cmd !! 0) (tail cmd) $ \read_input write_output -> do
    -- Give some time for command to warm up (like connecting to NAO)
    threadDelay 2000000

    world_state_tvar <- newTVarIO Nothing

    let world_state_stm = readTVar world_state_tvar >>= \case
                            Nothing -> retry
                            Just world_state -> return world_state

        report_world ws = writeTVar world_state_tvar $ Just ws

    withAsync (runWebDiagnostics world_state_stm config) $ \web_diagnostics -> do
      link web_diagnostics

      activity_report_tvar <- newTVarIO (ActivityReport { lastActivityObserved = 0
                                                        , endOfDataObserved = False
                                                        , pendingDataSend = False
                                                        , screenState = emptyScreenState 80 24
                                                        , processingInput = False
                                                        , cursorX = 0
                                                        , cursorY = 0 }, Nothing)

      let write_outputter = forever $ do
                              -- We do two transactions to make sure `now` is up to date when we want to update lastActivityObserved variable
                              atomically $ do
                                (_ar, sending) <- readTVar activity_report_tvar
                                case sending of
                                  Nothing -> retry
                                  Just _ -> return ()
                              now <- toNanoSecs <$> getTime Monotonic
                              atomically $ do
                                (ar, sending) <- readTVar activity_report_tvar
                                case sending of
                                  Nothing -> return ()
                                  Just bs -> do
                                    write_output bs
                                    writeTVar activity_report_tvar (ar { lastActivityObserved = now, pendingDataSend = False, endOfDataObserved = False }, Nothing)
  
      withAsync write_outputter $ \write_async -> do
        liftIO $ link write_async
        withAsync (botEntry config activity_report_tvar report_world >> killThread tid) $ \bot_async -> do
          liftIO $ link bot_async
  
          withAsync (forever $ do
                      bs <- B.hGetSome stdin 1024
                      atomically $ write_output bs) $ \_ -> flip evalStateT (emptyScreenState 80 24, emulator, 0, 0) $ forever $ do
  
            bs <- liftIO $ atomically read_input
            -- Update lastActivityObserved ASAP
            now <- liftIO $ toNanoSecs <$> getTime Monotonic
            liftIO $ atomically $ modifyTVar activity_report_tvar $ \(ar, sending) -> (ar { lastActivityObserved = now, processingInput = True }, sending)
  
            (st, emu, cx, cy) <- get
  
            let ((new_st, new_emu, new_cx, new_cy), stm) = runST $ do
                                      thawed <- thawScreen st
                                      exhaust thawed st bs emu cx cy activity_report_tvar
  
  	  -- Force evaluation of all things (we have to be able to trust the
  	  -- `now` and `processingInput` assignments below to reflect that
  	  -- input actually was processed)
            void $ liftIO $ evaluate $ force $ new_st `deepseq` new_cx `deepseq` new_cy `deepseq` ()
  
            liftIO $ do
              now <- toNanoSecs <$> getTime Monotonic
              atomically $ do
                modifyTVar activity_report_tvar $ \(ar, sending) ->
                  (ar { lastActivityObserved = now
                      , screenState = new_st
                      , cursorX = new_cx
                      , cursorY = new_cy
                      , processingInput = False }, sending)
                stm
    
            put (new_st, new_emu, new_cx, new_cy)
            liftIO $ do
              BL.putStr (toANSIOutput Nothing new_st)
              hFlush stdout
 where
  exhaust :: ScreenStateMut s
          -> ScreenState
          -> B.ByteString
          -> Emulator ()
          -> Int
          -> Int
	  -> TVar (ActivityReport, Maybe B.ByteString)
          -> ST s ((ScreenState, Emulator (), Int, Int), STM ())
  exhaust thawed st bs emu cx cy activity_report_tvar = do
    case emu of
      Pure () -> logError "impossible!"

      Free (UpdateCursorPosition x y next) ->
        exhaust thawed st bs next x y activity_report_tvar

      Free (YieldEndOfData next) -> do
        (r, io) <- exhaust thawed st bs next cx cy activity_report_tvar
        return (r, do io
		      (ar, st) <- readTVar activity_report_tvar
		      writeTVar activity_report_tvar (ar { endOfDataObserved = True }, st))

      Free (ReadByte fun) ->
        if B.null bs
          then do new_st <- freezeScreen thawed
                  return ((new_st, emu, cx, cy), return ())
          else exhaust thawed st (B.tail bs) (fun $ B.head bs) cx cy activity_report_tvar

      Free (PeekByte fun) -> do
        if B.null bs
          then do new_st <- freezeScreen thawed
                  return ((new_st, emu, cx, cy), return ())
          else exhaust thawed st bs (fun $ B.head bs) cx cy activity_report_tvar

      Free (YieldChange mut next) -> do
        mut thawed >> exhaust thawed st bs next cx cy activity_report_tvar

      Free (GetCurrentState fun)  -> do
        exhaust thawed st bs (fun st) cx cy activity_report_tvar

