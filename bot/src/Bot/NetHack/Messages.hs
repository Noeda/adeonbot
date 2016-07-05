{-# LANGUAGE OverloadedStrings #-}

module Bot.NetHack.Messages
  ( consumeMessages )
  where

import Bot.NetHack.MonadAI
import Data.Monoid
import qualified Data.Text as T

-- | Presses space until all messages have been consumed.
--
-- Returns all the messages saw on the screen.
consumeMessages :: MonadAI m => m [T.Text]
consumeMessages = do
  topline <- getScreenLine 0
  -- Break the top line from places where double spaces occur.
  -- Not 100% watertight but it should keep most relevant messages properly
  -- separated.
  let msgs = fmap T.strip $ filter (not . T.null) $ T.splitOn "  " topline

  -- Is there a "--More--" on the screen? Press space is yes
  has_more <- matchf "--More--"
  if has_more
    then send " " >> (msgs <>) <$> consumeMessages
    else return msgs

