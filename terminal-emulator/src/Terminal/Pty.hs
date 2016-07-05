-- | Run processes inside pseudo-terminals.

{-# LANGUAGE LambdaCase #-}

module Terminal.Pty
  ( withProcessInPty
  , Terminal.Pty.killProcess
  , CreateProcess(..) )
  where

import qualified Data.ByteString as B
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import System.IO
import System.Posix.Signals
import System.Posix.Terminal.ByteString
import System.Process
import System.Process.Internals

killProcess :: ProcessHandle -> IO ()
killProcess (ProcessHandle mvar _) = withMVar mvar $ \case
  OpenHandle pid -> signalProcessGroup sigKILL pid
  ClosedHandle {} -> return ()

-- | Like `createProcess` but sets stdin, stdout and stderr according to a
-- pseudo-terminal.
withProcessInPty :: CreateProcess -> (STM B.ByteString -> (B.ByteString -> STM ()) -> IO a) -> IO a
withProcessInPty process' action = mask $ \restore -> do
  (master, slave) <- openPseudoTerminal
  slavehd <- fdToHandle (fromIntegral slave)
  masterhd <- fdToHandle (fromIntegral master)
  
  flip finally (hClose masterhd >> hClose slavehd) $ do

    let process = process' {
                    std_in = UseHandle slavehd
                  , std_out = UseHandle slavehd
                  , std_err = UseHandle slavehd
                  , create_group = True
                  , new_session = True }

    inbox <- newEmptyTMVarIO
    outbox <- newEmptyTMVarIO
    tid <- myThreadId

    let inputLoop = forever $ do
                      i <- atomically $ takeTMVar inbox
                      B.hPutStr masterhd i

        outputLoop = forever $ do
                       bs <- B.hGetSome masterhd 4096
                       when (B.null bs) $ do
                         killThread tid
                         mytid <- myThreadId
                         killThread mytid
                       atomically $ putTMVar outbox bs

    (_, _, _, phandle) <- createProcess process
    flip finally (Terminal.Pty.killProcess phandle >> void (waitForProcess phandle)) $ do
      hClose slavehd
      withAsync inputLoop $ \iloop ->
        withAsync outputLoop $ \oloop -> do
          link iloop
          link oloop
          restore (action (takeTMVar outbox) (putTMVar inbox))

