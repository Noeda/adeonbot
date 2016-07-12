{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module Bot.NetHack.AIEntry
  ( AIState()
  , doesAILookLikeItsOscillating
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
import Data.Monoid
import qualified Data.IntMap.Strict as IM
import qualified Data.Text.Encoding as T
import Data.Time
import Terminal.Screen

data AIState = AIState
  { _botConfig :: !BotConfig
  , _nextAction :: !(AI ())
  , _turnCountLastChanged :: !(Turn, UTCTime) }
makeLenses ''AIState

doesAILookLikeItsOscillating :: MonadIO m => AIState -> m Bool
doesAILookLikeItsOscillating ai = liftIO $ do
  now <- getCurrentTime
  return $ diffUTCTime now (ai^.turnCountLastChanged._2) > 20

emptyAIState :: MonadIO m => BotConfig -> m AIState
emptyAIState bc = liftIO $ do
  now <- getCurrentTime
  return $ AIState
          { _botConfig = bc
          , _nextAction = bot bc
          , _turnCountLastChanged = (1, now) }

stepAIState :: UTCTime -> ScreenState -> Int -> Int -> AIState -> (AIState, B.ByteString)
stepAIState now screenstate x y aistate =
  runAI now screenstate x y aistate (fromFT $ aistate^.nextAction)

runAI :: UTCTime -> ScreenState -> Int -> Int -> AIState -> (FreeT AIF Identity ()) -> (AIState, B.ByteString)
runAI now screenstate w h aistate' (FreeT (Identity ai)) = case ai of
  Pure () -> (aistate, B.empty)
  Free (ReportWorldState ws next) ->
    if ws^.turn > aistate^.turnCountLastChanged._1
      then runAI now screenstate w h (aistate & turnCountLastChanged .~ (ws^.turn, now)) next
      else runAI now screenstate w h aistate next
  Free (Yield next) -> runAI now screenstate w h aistate next
  Free (GetCurrentScreenState fun) -> runAI now screenstate w h aistate (fun screenstate w h)
  Free (Send bs next) -> (aistate & nextAction .~ (toFT next), bs)
  Free (SendRaw bs next) -> (aistate & nextAction .~ (toFT next), bs)
 where
  aistate = aistate' & nextAction .~ (return ())

serverLogin :: MonadAI m => BotConfig -> m Bool
serverLogin bc = matchfirst $
  [ ("l) Login", send "l")
  , ("Please enter your username", send ( (T.encodeUtf8 $ playername bc) <> "\n" ))
  , ("1) Go to NetHack 3.6.0 menu", send "1")   -- NAO
  , ("p) Play NetHack 3.6.0", send "p") ] ++ password_item
 where
  password_item = case password bc of
    Nothing -> [ ("Please enter your password", error "serverLogin: password asked but not set in yaml config.") ]
    Just pw -> [ ("Please enter your password", send ( T.encodeUtf8 pw <> "\n" )) ]

characterCreation :: MonadAI m => m Bool
characterCreation = matchfirst
  [ ("Shall I pick a character's", send "n")
  , ("Pick a role or profession", send "v")
  , ("Pick a race or species", send "d")
  , ("Pick an alignment", send "l")
  , ("Is this ok?", send "y")
  , ("It is written in the Book of", send " ") ]

bot :: BotConfig -> AI ()
bot bc = do
  send " "
  repeatUntilFalse $ serverLogin bc
  repeatUntilFalse characterCreation

  worldLoop emptyWorldState IM.empty decisionMaker
 where
  worldLoop old_state answermap maker = do
    reportWorldState old_state

    (new_state1, msgs) <- exhaustMessages old_state (fromFT $ runWAI $ consumeMessages answermap)

    reportWorldState new_state1

    new_state2 <- inferWorldState msgs new_state1

    reportWorldState new_state2

    exhaust new_state2 msgs answermap (fromFT $ runWAI maker)
   where
    exhaustMessages new_state (FreeT maker) = do
      (item, (new_state, _, _)) <- runStateT maker (new_state, [], IM.empty)
      case item of
        Pure msgs -> return (new_state, msgs)

        Free (ReportWorldState _ next) -> exhaustMessages new_state next

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

        Free (ReportWorldState _ next) -> exhaust new_state msgs answermap next

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

