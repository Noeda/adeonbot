module Bot.NetHack.Logs
  ( logTrace
  , logError )
  where

import Control.Concurrent.MVar
import Data.Monoid
import Data.Time
import System.IO
import System.IO.Unsafe

putLogLine :: String -> Handle -> IO ()
putLogLine line handle = do
  now <- getCurrentTime
  hPutStrLn handle $ show now <> ": " <> line
  hFlush handle

botlogHandle :: MVar Handle
botlogHandle = unsafePerformIO $ do
  f <- openFile "botlog.txt" AppendMode
  putLogLine "Log opened" f
  newMVar f

logError :: String -> a
logError str =
  let x = unsafePerformIO $ withMVar botlogHandle $ \handle -> do
            putLogLine ("ERROR: " <> str) handle
   in x `seq` error str

-- | Magic function that causes a line to be written in a logfile before
-- returning the result.
logTrace :: String -> a -> a
logTrace line x = unsafePerformIO $ withMVar botlogHandle $ \handle -> do
  putLogLine line handle
  return x

