{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.NetHack
  ( runAdeonbot )
  where

import Bot.NetHack.AIEntry
import Bot.NetHack.Config
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.ST
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Monoid
import qualified Data.Text as T
import Data.Yaml
import System.Clock
import System.Environment
import System.Exit
import System.IO
import Terminal.Emulator
import Terminal.Pty
import Terminal.Screen
import Terminal.Terminal

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

botEntry :: BotConfig -> STM (ScreenState, Int, Int, Integer) -> IO () -> (B.ByteString -> STM ()) -> IO ()
botEntry config getNextStatus refresh send = aiLoop (emptyAIState config)
 where
  aiLoop aistate = do
    status <- waitUntilCooldown
    new_aistate <- botLogic status aistate
    refresh
    aiLoop new_aistate

  waitUntilCooldown = do
    now <- toNanoSecs <$> getTime Monotonic
    let accepted_time = now - 40000000 -- 40ms

    maybestatus <- atomically $ do
      st@(_, _, _, timing) <- getNextStatus
      return $ if timing > accepted_time
        then Nothing
        else Just st

    case maybestatus of
      Nothing -> do
        threadDelay 10000 -- 10ms
        waitUntilCooldown
      Just status -> return status

  botLogic (screenstate, cx, cy, _) aistate = do
    let (new_aistate, sending) = stepAIState screenstate cx cy aistate
    if B.null sending
      then error "botEntry: AI decided not to send anything to NetHack. Panic."
      else atomically $ send sending
    return new_aistate

run :: BotConfig -> IO ()
run config = withRawTerminalMode $ do
  let cmd = fmap T.unpack $ nethackCommand config

  now <- toNanoSecs <$> getTime Monotonic
  status_tvar <- newTVarIO (emptyScreenState 80 24, 0, 0, now)

  let get_next_status = readTVar status_tvar
      refresh = do now <- toNanoSecs <$> getTime Monotonic
                   atomically $ modifyTVar status_tvar $ \(x, y, z, _) -> (x, y, z, now)

  tid <- myThreadId

  withProcessInPty (cmd !! 0) (tail cmd) $ \read_input write_output ->
    withAsync (botEntry config get_next_status refresh write_output >> killThread tid) $ \bot_async -> do
      liftIO $ link bot_async
      withAsync (forever $ do
                  bs <- B.hGetSome stdin 1024
                  atomically $ write_output bs) $ \_ -> flip evalStateT (emptyScreenState 80 24, emulator, 0, 0) $ forever $ do

        bs <- liftIO $ atomically read_input
        (st, emu, cx, cy) <- get
        let (new_st, new_emu, new_cx, new_cy) = runST $ do
                                  thawed <- thawScreen st
                                  exhaust thawed st bs emu cx cy
        put (new_st, new_emu, new_cx, new_cy)
        liftIO $ do
          BL.putStr (toANSIOutput Nothing new_st)
          hFlush stdout
          now <- toNanoSecs <$> getTime Monotonic
          atomically $ writeTVar status_tvar (new_st, new_cx, new_cy, now)
 where
  exhaust :: ScreenStateMut s
          -> ScreenState
          -> B.ByteString
          -> Emulator ()
          -> Int
          -> Int
          -> ST s (ScreenState, Emulator (), Int, Int)
  exhaust thawed st bs emu cx cy = do
    case emu of
      Pure () -> error "impossible!"

      Free (UpdateCursorPosition x y next) ->
        exhaust thawed st bs next x y

      Free (ReadByte fun) ->
        if B.null bs
          then do new_st <- freezeScreen thawed
                  return (new_st, emu, cx, cy)
          else exhaust thawed st (B.tail bs) (fun $ B.head bs) cx cy

      Free (PeekByte fun) -> do
        if B.null bs
          then do new_st <- freezeScreen thawed
                  return (new_st, emu, cx, cy)
          else exhaust thawed st bs (fun $ B.head bs) cx cy

      Free (YieldChange mut next) -> do
        mut thawed >> exhaust thawed st bs next cx cy
      Free (GetCurrentState fun)  -> do
        exhaust thawed st bs (fun st) cx cy

