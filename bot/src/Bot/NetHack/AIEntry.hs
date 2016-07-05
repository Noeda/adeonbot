{-# LANGUAGE TemplateHaskell #-}

module Bot.NetHack.AIEntry
  ( AIState()
  , emptyAIState
  , stepAIState )
  where

import Bot.NetHack.Config
import qualified Data.ByteString as B
import Control.Lens
import Terminal.Screen

data AIState = AIState
  { _botConfig :: !BotConfig }
makeLenses ''AIState

emptyAIState :: BotConfig -> AIState
emptyAIState bc = AIState { _botConfig = bc }

stepAIState :: ScreenState -> Int -> Int -> AIState -> (AIState, B.ByteString)
stepAIState screenstate x y aistate = (aistate, B.empty)

