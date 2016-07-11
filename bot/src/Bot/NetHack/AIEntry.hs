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
import qualified Data.IntMap.Strict as IM
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
  Free (Yield next) -> runAI screenstate w h aistate next
  Free (GetCurrentScreenState fun) -> runAI screenstate w h aistate (fun screenstate w h)
  Free (Send bs next) -> (aistate & nextAction .~ (toFT next), bs)
  Free (SendRaw bs next) -> (aistate & nextAction .~ (toFT next), bs)
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

  worldLoop emptyWorldState IM.empty decisionMaker
 where
  worldLoop old_state answermap maker = do
    (new_state1, msgs) <- exhaustMessages old_state (fromFT $ runWAI $ consumeMessages answermap)
    new_state2 <- inferWorldState msgs new_state1

    exhaust new_state2 msgs answermap (fromFT $ runWAI maker)
   where
    exhaustMessages new_state (FreeT maker) = do
      (item, (new_state, _, _)) <- runStateT maker (new_state, [], IM.empty)
      case item of
        Pure msgs -> return (new_state, msgs)
        Free (Yield next) ->
          exhaustMessages new_state next
        Free (SendRaw bs next) -> do
          send bs
          exhaustMessages new_state next
        Free (Send bs next) -> do
          send bs
          exhaustMessages new_state next
        Free (GetCurrentScreenState fun) -> do
          (ss, cx, cy) <- currentScreen
          exhaustMessages new_state (fun ss cx cy)

    exhaust new_state msgs answermap (FreeT maker) = do
      (item, (new_world, _, new_answermap)) <- runStateT maker (new_state, msgs, answermap)
      case item of
        Pure () -> error "decisionMaker ran out."
        Free (Yield next) ->
          worldLoop new_world new_answermap (toWAI $ toFT next)
        Free (SendRaw bs next) -> do
          send bs
          exhaust new_state msgs new_answermap next
        Free (Send bs next) -> do
          send bs
          worldLoop new_world new_answermap (toWAI $ toFT next)
        Free (GetCurrentScreenState fun) -> do
          (ss, cx, cy) <- currentScreen
          exhaust new_world msgs new_answermap (fun ss cx cy)

