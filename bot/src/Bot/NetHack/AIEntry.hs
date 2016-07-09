{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module Bot.NetHack.AIEntry
  ( AIState()
  , botConfig
  , emptyAIState
  , stepAIState )
  where

import Bot.NetHack.Config
import Bot.NetHack.DecisionMaker
import Bot.NetHack.Messages
import Bot.NetHack.MonadAI
import Bot.NetHack.InferWorldState
import Bot.NetHack.WorldState
import Control.Lens
import Control.Monad.State.Strict
import Control.Monad.Trans.Free
import Control.Monad.Trans.Free.Church
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
stepAIState screenstate x y aistate =
  runAI screenstate x y aistate (fromFT $ aistate^.nextAction)

runAI :: ScreenState -> Int -> Int -> AIState -> (FreeT AIF Identity ()) -> (AIState, B.ByteString)
runAI screenstate w h aistate' (FreeT (Identity ai)) = case ai of
  Pure () -> (aistate, B.empty)
  Free (GetCurrentScreenState fun) -> runAI screenstate w h aistate (fun screenstate w h)
  Free (Send bs next) -> (aistate & nextAction .~ (toFT next), bs)
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

  worldLoop emptyWorldState decisionMaker
 where
  worldLoop old_state maker = do
    msgs <- consumeMessages
    new_state <- inferWorldState old_state

    exhaust new_state msgs (fromFT $ runWAI maker)
   where
    exhaust new_state msgs (FreeT maker) = do
      (item, (new_world, _)) <- runStateT maker (new_state, msgs)
      case item of
        Pure () -> error "decisionMaker ran out."
        Free (Send bs next) -> do
          send bs
          worldLoop new_world (toWAI $ toFT next)
        Free (GetCurrentScreenState fun) -> do
          (ss, cx, cy) <- currentScreen
          exhaust new_world msgs (fun ss cx cy)

