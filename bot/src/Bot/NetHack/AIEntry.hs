{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module Bot.NetHack.AIEntry
  ( AIState()
  , emptyAIState
  , stepAIState )
  where

import Bot.NetHack.Config
import Bot.NetHack.Messages
import Bot.NetHack.MonadAI
import Bot.NetHack.InferWorldState
import Bot.NetHack.WorldState
import Control.Lens
import Control.Monad
import Control.Monad.Free
import qualified Data.ByteString as B
import Terminal.Screen

data AIState = AIState
  { _botConfig :: !BotConfig
  , _nextAction :: !(AI ()) }
makeLenses ''AIState

emptyAIState :: BotConfig -> AIState
emptyAIState bc = AIState { _botConfig = bc
                          , _nextAction = bot }

stepAIState :: ScreenState -> Int -> Int -> AIState -> (AIState, B.ByteString)
stepAIState screenstate x y aistate = runAI screenstate x y aistate (aistate^.nextAction)

runAI :: ScreenState -> Int -> Int -> AIState -> AI () -> (AIState, B.ByteString)
runAI screenstate w h aistate' ai = case ai of
  Pure () -> (aistate, B.empty)
  Free (GetCurrentScreenState fun) -> runAI screenstate w h aistate (fun screenstate w h)
  Free (Send bs next) -> (aistate & nextAction .~ next, bs)
 where
  aistate = aistate' & nextAction .~ (return ())

characterCreation :: MonadAI m => m Bool
characterCreation = matchfirst
  [ ("Shall I pick a character's", send "n")
  , ("Pick a role or profession", send "v")
  , ("Pick a race or species", send "d")
  , ("Pick an alignment", send "l")
  , ("Is this ok?", send "y")
  , ("It is written in the Book of", send " ") ]

bot :: AI ()
bot = do
  repeatUntilFalse characterCreation

  worldLoop emptyWorldState
 where
  worldLoop old_state = do
    msgs <- consumeMessages
    new_state <- inferWorldState old_state
    send " "
    worldLoop new_state

