{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.NetHack.Config
  ( BotConfig(..) )
  where

import Control.Monad
import Data.Data
import qualified Data.Text as T
import Data.Word
import Data.Yaml
import GHC.Generics

data BotConfig = BotConfig
  { playername :: !T.Text
  , latency :: !Int
  , password :: !(Maybe T.Text)
  , nethackCommand :: [T.Text]
  , webDiagnosticsPort :: !Word16
  , webDiagnosticsHost :: !T.Text }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

instance FromJSON BotConfig where
  parseJSON (Object ob) = do
    pn <- ob .: "name"
    pw <- ob .: "password"
    lat <- ob .: "latency"
    cmd <- ob .: "nethackCommand"
    port <- ob .: "webDiagnosticsPort"
    host <- ob .: "webDiagnosticsHost"
    when (null cmd) $ fail "FromJSON.BotConfig: nethackCommand cannot be empty."
    return BotConfig { playername = pn
                     , password = pw
                     , latency = lat
                     , nethackCommand = cmd
                     , webDiagnosticsPort = port
                     , webDiagnosticsHost = host }

  parseJSON _ = fail "FromJSON.BotConfig: expected an object."

