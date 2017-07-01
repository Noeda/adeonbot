{-# LANGUAGE OverloadedStrings #-}

-- | This module implements generic logic for choosing items from a list.
--
-- Used for eating, dropping, picking items and so on...
--

module Bot.NetHack.SelectItem
  ( selectItem
  , selectManyItems )
  where

import Bot.NetHack.InferWorldState
import Bot.NetHack.MonadAI
import Bot.NetHack.ScreenPattern
import Bot.NetHack.WorldState
import Control.Monad
import qualified Data.ByteString as B
import Data.List ( find )
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Traversable ( for )

looksLikeItemLine :: T.Text -> Bool
looksLikeItemLine txt =
  T.length txt >= 5 &&
  T.index txt 1 == ' ' &&
  T.index txt 2 == '-' &&
  T.index txt 3 == ' '

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
  bottom_item <- matchf (regex "\\([0-9]+ of [0-9]+\\)|\\(end\\)")
  case bottom_item of
    Nothing -> return Nothing
    Just dd -> do
      items <- fmap catMaybes $ for [0..y dd-1] $ \row -> do
        item_line <- T.strip <$> getScreenLine' row (x dd)
        return $
          if looksLikeItemLine item_line
            then Just $ (T.head item_line, nameToItem $ T.drop 4 item_line)
            else Nothing

      case find (\(_, item) -> item_selector item) items of
        Nothing -> do
          sendRaw " "
          selectItem item_selector
        Just (letter, _) -> return $ Just $ T.encodeUtf8 (T.singleton letter)

-- | Selects one or more items.
--
-- Returns the number of items picked. Like `selectItem`, this can and will
-- send space to NetHack to browse through the list.
--
-- Also takes an accumulator. You can choose how many items to pick up by
-- specifying an integer other than 1 (only works for stacks).
--
-- This will pick the items and press space but will NOT yield so there is no
-- turn handling or anything. Call `yield` after this manually. (not all cases
-- want to do `yield` immediately)
selectManyItems :: MonadWAI m
                => (Item -> a -> (Int, a))
                -> a
                -> m a
selectManyItems item_selector def = do
  bottom_item <- matchf (regex "\\([0-9]+ of [0-9]+\\)|\\(end\\)")
  case bottom_item of
    Nothing -> return def
    Just dd -> do
      items <- fmap catMaybes $ for [0..y dd-1] $ \row -> do
        item_line <- T.strip <$> getScreenLine' row (x dd)
        return $
          if looksLikeItemLine item_line
            then Just $ (T.head item_line, nameToItem $ T.drop 4 item_line)
            else Nothing

      (new_accumulator, bs) <- loop_selection def B.empty items
      unless (B.null bs) $
        sendRaw bs
      sendRaw " "

      selectManyItems item_selector new_accumulator
 where
  loop_selection accumulator bs [] = return (accumulator, bs)
  loop_selection accumulator bs ((letter, item):rest) =
    case item_selector item accumulator of
      (0, new_accumulator) ->
        loop_selection new_accumulator bs rest
      (n, _) | n < 0 -> error "selectManyItems: cannot select negative number of items."
      (n, new_accumulator) ->
        loop_selection new_accumulator (bs <> (T.encodeUtf8 $ T.pack $ show n) <> (T.encodeUtf8 $ T.singleton letter)) rest


