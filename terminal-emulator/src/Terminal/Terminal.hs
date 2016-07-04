{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Raw terminal handling.

module Terminal.Terminal
  ( withRawTerminalMode
  , getTerminalSize )
  where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import Data.Data
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

foreign import ccall free_err_msg :: Ptr CChar -> IO ()
foreign import ccall set_character_mode
    :: Ptr (Ptr CChar) -> IO (Ptr ())
foreign import ccall restore_character_mode :: Ptr () -> Ptr (Ptr CChar) -> IO CInt
foreign import ccall get_terminal_size :: Ptr CInt -> Ptr CInt -> IO ()

data TerminalError = TerminalError String
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Typeable )

instance Exception TerminalError

-- | Runs an IO action inside a non-echoing raw mode.
withRawTerminalMode :: (MonadIO m, MonadMask m) => m a -> m a
withRawTerminalMode action = mask $ \restore -> do
  ac <- liftIO $ alloca $ \err_ptrptr -> do
    tpointer <- set_character_mode err_ptrptr
    if tpointer == nullPtr

      then do err_ptr <- peek err_ptrptr
              err <- B.packCString err_ptr
              free_err_msg err_ptr
              return $ throwM $ TerminalError $ T.unpack $ T.decodeUtf8 err

      else return $ flip finally

             (liftIO $ do
               alloca $ \err_ptrptr -> do
                 result <- restore_character_mode tpointer err_ptrptr
                 unless (result == 0) $ do
                   err_ptr <- peek err_ptrptr
                   err <- B.packCString err_ptr
                   throwM $ TerminalError $ T.unpack $ T.decodeUtf8 err)

             (do x <- restore action
                 return x)
  ac

-- | Gets the size of the terminal we are running in.
getTerminalSize :: IO (Int, Int)
getTerminalSize = alloca $ \w_ptr -> alloca $ \h_ptr -> do
  get_terminal_size w_ptr h_ptr
  w <- peek w_ptr
  h <- peek h_ptr
  return (fromIntegral w, fromIntegral h)

