{-# LANGUAGE OverloadedStrings #-}

module Bot.NetHack.WebDiagnostics
  ( runWebDiagnostics )
  where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Bot.NetHack.Config
import Bot.NetHack.WorldState
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.String
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import qualified Data.Text as T

runWebDiagnostics :: STM WorldState -> BotConfig -> IO ()
runWebDiagnostics world_state_get config = do

  chan <- newBroadcastTChanIO

  withAsync (worldChangeDetector world_state_get chan) $ \world_changer_async -> do
    link world_changer_async

    let settings = setPort (fromIntegral $ webDiagnosticsPort config) $
                   setHost (fromString $ T.unpack $ webDiagnosticsHost config) $
                   defaultSettings

    runSettings settings (app chan)

worldChangeDetector :: STM WorldState -> TChan (WorldState, BL.ByteString) -> IO ()
worldChangeDetector world_state_get world_state_chan = do
  -- This function sends WorldState to given channel if it detects world has
  -- changed.
  first_world <- atomically $ do
    first_world <- world_state_get
    writeTChan world_state_chan (first_world, encode first_world)
    return first_world

  go first_world
 where
  go previous_world = do
    next_world <- atomically $ do
      next_world <- world_state_get
      when (next_world == previous_world) retry

      writeTChan world_state_chan (next_world, encode next_world)
      return next_world
    go next_world

app :: TChan (WorldState, BL.ByteString) -> Application
app world_state_chan req respond = case pathInfo req of
  ["api", "v1", "websocket", "botfeed"] -> botFeed world_state_chan req respond
  _ -> respond $ responseLBS status404 [] "Not found."

botFeed :: TChan (WorldState, BL.ByteString) -> Application
botFeed world_state_chan = websocketsOr defaultConnectionOptions (botFeedWS world_state_chan) $ \_ respond ->
  respond $ responseLBS status400 [] "This endpoint only accepts WebSocket connections."

botFeedWS :: TChan (WorldState, BL.ByteString) -> ServerApp
botFeedWS world_state_chan pending_connection = do
  conn <- acceptRequest pending_connection
  withAsync (connReader conn) $ \reader_async -> do
    link reader_async

    dupped_chan <- atomically $ dupTChan world_state_chan

    forever $ do
      (_world_state, json) <- atomically $ readTChan dupped_chan
      sendTextData conn json

connReader :: Connection -> IO ()
connReader conn = forever $
  -- This will throw exception if CloseRequest is sent
  void $ receiveDataMessage conn

