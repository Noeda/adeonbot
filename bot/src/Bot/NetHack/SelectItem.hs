{-# LANGUAGE OverloadedStrings #-}

-- | This module implements generic logic for choosing items from a list.
--
-- Used for eating, dropping, picking items and so on...
--

module Bot.NetHack.SelectItem
  ( selectItem )
  where

import Bot.NetHack.InferWorldState
import Bot.NetHack.MonadAI
import Bot.NetHack.ScreenPattern
import Bot.NetHack.WorldState
import qualified Data.ByteString as B
import Data.List ( find )
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Traversable ( for )

-- | Select an item from an item listing that chooses one single item (e.g.
-- eating e* )
--
-- Assumes there is an item listing on the screen.
--
-- This may press space to browser the item listing.
--
-- Returns letter to press.
selectItem :: MonadWAI m => (Item -> Bool) -> m (Maybe B.ByteString)
selectItem item_selector = do
  bottom_item <- matchf (regex "([0-9]+ of [0-9]+)|\\(end\\)")
  case bottom_item of
    Nothing -> return Nothing
    Just dd -> do
      items <- fmap catMaybes $ for [0..y dd-1] $ \row -> do
        item_line <- T.strip <$> getScreenLine' row (x dd)
        return $
          if T.length item_line >= 5 &&
             T.index item_line 1 == ' ' &&
             T.index item_line 2 == '-' &&
             T.index item_line 3 == ' '
            then Just $ (T.head item_line, nameToItem $ T.drop 4 item_line)
            else Nothing

      case find (\(_, item) -> item_selector item) items of
        Nothing -> do
          sendRaw " "
          selectItem item_selector
        Just (letter, _) -> return $ Just $ T.encodeUtf8 (T.singleton letter)

