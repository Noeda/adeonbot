{-# LANGUAGE OverloadedStrings #-}

module Bot.NetHack.Messages
  ( consumeMessages )
  where

import Bot.NetHack.MonadAI
import Bot.NetHack.ScreenPattern
import Control.Monad
import qualified Data.IntMap.Strict as IM
import Data.Monoid
import qualified Data.Text as T

import Debug.Trace

-- | Presses space until all messages have been consumed.
--
-- Returns all the messages saw on the screen.
consumeMessages :: MonadAI m => IM.IntMap (T.Text, m ()) -> m [T.Text]
consumeMessages answermap = do
  skipThingsThatAreHere
  msgs <- pluckMessages
  answerQuestions answermap
  return $ filter (not . T.null) msgs

answerQuestions :: MonadAI m => IM.IntMap (T.Text, m ()) -> m ()
answerQuestions answermap = do
  (_, _, cy) <- currentScreen
  more <- traceShow (fmap fst $ IM.elems $ answermap) $ matchf (limitRows [0,1,2] "--More--")

  -- Some kind of question on the screen?
  when (cy == 0 && more == False) $ do
    line <- getScreenLine 0
    let looper [] = return ()
        looper ((_, (text, action)):rest) = if T.isInfixOf text line
                                         then action
                                         else looper rest

    looper (IM.toDescList answermap)

skipThingsThatAreHere :: MonadAI m => m ()
skipThingsThatAreHere = do
  topline <- getScreenLine 0

  -- Skip "Things that are here:" item listing
  when (T.isInfixOf "Things that are here:" topline ||
        T.isInfixOf "Things that you feel here:" topline) $ do
    has_more <- matchf "--More--"
    when has_more $ send " "

pluckMessages :: MonadAI m => m [T.Text]
pluckMessages = do
  topline <- getScreenLine 0

  -- Break the top line from places where double spaces occur.
  -- Not 100% watertight but it should keep most relevant messages properly
  -- separated.
  let msgs = fmap T.strip $ filter (not . T.null) $ T.splitOn "  " topline

  -- Is there a "--More--" on the screen? Press space is yes
  has_more <- matchf "--More--"
  if has_more
    then send " " >> (msgs <>) <$> pluckMessages
    else return msgs

