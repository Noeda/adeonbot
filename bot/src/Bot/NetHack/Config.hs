{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.NetHack.Config
  ( BotConfig(..) )
  where

import Control.Monad
import Data.Data
import qualified Data.Text as T
import Data.Yaml
import GHC.Generics

data BotConfig = BotConfig
  { playername :: !T.Text
  , nethackCommand :: [T.Text] }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

instance FromJSON BotConfig where
  parseJSON (Object ob) = do
    pn <- ob .: "name"
    cmd <- ob .: "nethackCommand"
    when (null cmd) $ fail "FromJSON.BotConfig: nethackCommand cannot be empty."
    return BotConfig { playername = pn
                     , nethackCommand = cmd }

  parseJSON _ = fail "FromJSON.BotConfig: expected an object."

