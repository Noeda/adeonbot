{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Main ( main ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.ST
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import System.Environment
import System.IO
import System.Process
import Terminal.Emulator
import Terminal.Pty
import Terminal.Screen
import Terminal.Terminal

main :: IO ()
main = withRawTerminalMode $ do
  (w, h) <- getTerminalSize

  args <- getArgs

  withProcessInPty (proc (args !! 0) (tail args)) $ \read_input write_output ->
    withAsync (forever $ do
                 bs <- B.hGetSome stdin 1024
                 atomically $ write_output bs) $ \_ -> flip evalStateT (emptyScreenState w h, emulator) $ forever $ do
      bs <- liftIO $ atomically read_input
      (st, emu) <- get
      let (new_st, new_emu) = runST $ do
                                thawed <- thawScreen st
                                exhaust thawed st bs emu
      put (new_st, new_emu)
      liftIO $ do
        BL.putStr (toANSIOutput Nothing new_st)
        --new_st `seq` print (14, bs)
        hFlush stdout
 where
  exhaust :: ScreenStateMut s -> ScreenState -> B.ByteString -> Emulator () -> ST s (ScreenState, Emulator ())
  exhaust thawed st bs emu = do
    case emu of
      Pure () -> error "impossible!"
      Free (ReadByte fun) ->
        if B.null bs
          then do new_st <- freezeScreen thawed
                  return (new_st, emu)
          else exhaust thawed st (B.tail bs) (fun $ B.head bs)

      Free (PeekByte fun) -> do
        if B.null bs
          then do new_st <- freezeScreen thawed
                  return (new_st, emu)
          else exhaust thawed st bs (fun $ B.head bs)

      Free (YieldChange mut next) -> do
        mut thawed >> exhaust thawed st bs next
      Free (GetCurrentState fun)  -> do
        exhaust thawed st bs (fun st)

