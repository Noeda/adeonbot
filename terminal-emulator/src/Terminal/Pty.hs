-- | Run processes inside pseudo-terminals.

{-# LANGUAGE LambdaCase #-}

module Terminal.Pty
  ( withProcessInPty )
  where

import qualified Data.ByteString as B
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Int
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import System.IO
import System.Posix.Signals
import System.Posix.Process.ByteString
import System.Posix.Types
import System.Process.Internals

foreign import ccall make_pty_and_fork
  :: Ptr CChar -> Ptr (Ptr CChar) -> Ptr CInt -> CInt -> CInt -> Ptr CInt -> IO Int64

makeProcessInPty :: String -> [String] -> Int -> Int -> IO (Handle, CPid)
makeProcessInPty exe args w h = mask_ $
  withCString exe $ \exe_ptr ->
  withStrArray (exe:args) $ \args_ptr ->
  alloca $ \err_ptr ->
  alloca $ \masterfd_ptr -> do
    result <- make_pty_and_fork exe_ptr args_ptr err_ptr (fromIntegral w) (fromIntegral h) masterfd_ptr
    err <- peek err_ptr
    if result == -1
      then ioError (errnoToIOError "makeProcessInPty" (Errno err) Nothing (Just "Terminal.Pty"))
      else do masterfd <- peek masterfd_ptr
              handle <- fdToHandle masterfd
              return (handle, fromIntegral result)
 where
  withStrArray args action = do
    allocaArray (length args+1) $ \ptrarray ->
      layout ptrarray args (action ptrarray)

  layout ptr [] action = do
    poke ptr nullPtr
    action
  layout ptr (str:rest) action = withCString str $ \cstr -> do
    poke ptr cstr
    layout (plusPtr ptr (sizeOf (undefined :: Ptr CChar))) rest action

newtype PHandle = PHandle (MVar (Maybe CPid))

makePHandle :: CPid -> IO PHandle
makePHandle pid = mask_ $ do
  mvar <- newMVar (Just pid)

  let killer = do signalProcessGroup sigKILL pid
                  void $ getProcessStatus True False pid

  void $ mkWeakMVar mvar $ do
    v <- takeMVar mvar
    case v of
      Nothing -> return ()
      Just _ -> killer

  return $ PHandle mvar

closePHandle :: PHandle -> IO ()
closePHandle (PHandle mvar) = mask_ $ modifyMVar_ mvar $ \case
  Nothing -> return Nothing
  Just pid -> do
    signalProcessGroup sigKILL pid
    void $ getProcessStatus True False pid
    return Nothing

withProcessInPty :: String -> [String] -> (STM B.ByteString -> (B.ByteString -> STM ()) -> IO a) -> IO a
withProcessInPty exe args action = mask $ \restore -> do
  (masterhd, pid) <- makeProcessInPty exe args 80 24
  phandle <- makePHandle pid

  flip finally (closePHandle phandle >> hClose masterhd) $ restore $ do
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

    withAsync inputLoop $ \iloop ->
      withAsync outputLoop $ \oloop -> do
        link iloop
        link oloop
        action (takeTMVar outbox) (putTMVar inbox)

