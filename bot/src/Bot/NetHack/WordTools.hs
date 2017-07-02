{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Bot.NetHack.WordTools
  ( stripAThe
  , stripInvisible
  , stripMonsterPrefixes )
  where

import qualified Data.Text as T

stripMonsterPrefixes :: T.Text -> T.Text
stripMonsterPrefixes = stripInvisible . stripAThe

-- | Strips a/an/the from beginning of texts
stripAThe :: T.Text -> T.Text
stripAThe txt' =
  let txt = T.strip txt'
   in if | T.isPrefixOf "the " (T.toLower txt)
           -> T.drop 4 txt
         | T.isPrefixOf "a " (T.toLower txt)
           -> T.drop 2 txt
         | T.isPrefixOf "an " (T.toLower txt)
           -> T.drop 3 txt
         | otherwise -> txt

-- | Strips the word 'invisible' out of text beginning
stripInvisible :: T.Text -> T.Text
stripInvisible txt' =
  let txt = T.strip txt'
   in if | T.isPrefixOf "invisible " (T.toLower txt)
           -> T.drop 10 txt
         | otherwise
           -> txt

