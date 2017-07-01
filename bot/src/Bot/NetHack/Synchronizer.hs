{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.NetHack.Synchronizer
  ( Sender()
  , BotLogicRunner()
  , ActivitySyncer()
  , ActivityReport(..)
  , makeActivitySyncer
  , makeSender
  , makeBotLogicRunner
  , ToleratedLatency
  , runSynchronizer )
  where

import Bot.NetHack.Logs
import Control.Concurrent ( threadDelay )
import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import qualified Data.ByteString as B
import Data.Data
import Data.Monoid
import GHC.Generics
import System.Clock
import Terminal.Screen

type ToleratedLatency = Integer

newtype BotLogicRunner state = BotLogicRunner (state -> ScreenState -> Int -> Int -> IO (state, B.ByteString))

newtype Sender = Sender (B.ByteString -> IO ())

data ActivityReport = ActivityReport
  { lastActivityObserved :: !Integer
  , endOfDataObserved    :: !Bool
  , pendingDataSend      :: !Bool
  , screenState          :: !ScreenState
  , cursorX              :: !Int
  , cursorY              :: !Int
  , processingInput      :: !Bool }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, NFData )

newtype ActivitySyncer = ActivitySyncer (IO ActivityReport)

makeActivitySyncer :: IO ActivityReport -> ActivitySyncer
makeActivitySyncer = ActivitySyncer

makeSender :: (B.ByteString -> IO ()) -> Sender
makeSender = Sender

makeBotLogicRunner :: (state -> ScreenState -> Int -> Int -> IO (state, B.ByteString)) -> BotLogicRunner state
makeBotLogicRunner = BotLogicRunner

runSynchronizer :: BotLogicRunner state -> Sender -> ActivitySyncer -> ToleratedLatency -> state -> IO ()
runSynchronizer (BotLogicRunner bot_logic) (Sender sender) activity_syncer latency initial_state = loop_it initial_state
 where
  loop_it state = do
    (screen_state, cx, cy) <- waitUntilCooldown latency activity_syncer
    (new_state, sending) <- bot_logic state screen_state cx cy
    logTrace ("Sending " <> show sending) $ sender sending
    loop_it new_state

waitUntilCooldown :: ToleratedLatency -> ActivitySyncer -> IO (ScreenState, Int, Int)
waitUntilCooldown latency (ActivitySyncer get_last_activity) = loop_it
 where
  loop_it = do
    result <- runMaybeT $ do
      report <- liftIO get_last_activity
      -- Don't proceed until the data we sent out previous iteration has been sent.
      guard (pendingDataSend report == False)
      -- Don't proceed until we are no longer processing data received from game
      guard (processingInput report == False)

      now <- liftIO $ toNanoSecs <$> getTime Monotonic

      let time_between_last_activity = now - lastActivityObserved report
          tolerance_nsecs = if endOfDataObserved report
                              then min latency 40000000 -- 40ms
       			      else latency


      -- Stop if it's been too short time since last activity observed from connection
      guard (time_between_last_activity >= tolerance_nsecs)

      return (screenState report, cursorX report, cursorY report)

    case result of
      Nothing -> threadDelay 10000 >> loop_it
      Just inner_result -> return inner_result

