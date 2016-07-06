{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.NetHack.ScreenPattern
  ( ScreenPattern()
  , Match(..)
  , Detailed(..)
  , match
  , regex
  , limitRows
  , debugPattern )
  where

import Data.Data
import Data.Foldable
import qualified Data.Set as S
import Data.String
import qualified Data.Text as T
import GHC.Generics
import Prelude hiding ( getLine )
import Terminal.Screen
import Text.Regex.TDFA hiding ( match )

data ScreenPattern = 
    Plain !T.Text
  | RegexPattern !Regex
  | RowLimited !(S.Set Int) !ScreenPattern
  deriving ( Typeable, Generic )

instance IsString ScreenPattern where
  fromString str = Plain $ T.pack str

limitRows :: Foldable f => f Int -> ScreenPattern -> ScreenPattern
limitRows ff pattern = RowLimited (S.fromList $ toList ff) pattern
{-# INLINE limitRows #-}

regex :: T.Text -> ScreenPattern
regex txt = RegexPattern $ makeRegex $ T.unpack txt
{-# INLINE regex #-}

data Detailed = Detailed
  { x :: !Int
  , y :: !Int
  , w :: !Int
  , h :: !Int
  , matches :: [T.Text] }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

class Match match where
  fromDetailed :: Maybe Detailed -> match

instance Match (Maybe Detailed) where
  fromDetailed = id

instance Match Bool where
  fromDetailed (Just{}) = True
  fromDetailed Nothing = False

instance Match [T.Text] where
  fromDetailed Nothing = []
  fromDetailed (Just d) = matches d

instance Match [String] where
  fromDetailed Nothing = []
  fromDetailed (Just d) = fmap T.unpack $ matches d

eligibleRows :: ScreenState -> ScreenPattern -> S.Set Int
eligibleRows ss pattern = case pattern of
  RowLimited rows payload ->
    S.intersection (S.intersection rows screenrows) (eligibleRows ss payload)
  _ -> screenrows
 where
  (_, sh) = screenSize ss
  screenrows = S.fromList [0..sh-1]

match :: Match match => ScreenState -> ScreenPattern -> match
match ss pattern = fromDetailed $ goOverLines eligible_rows
 where
  eligible_rows = eligibleRows ss pattern

  goOverLines rowset | Just (row, rest) <- S.minView rowset =
    let (txt, indexer) = getLine row ss
     in case patternTest pattern txt row indexer of
          Nothing -> goOverLines rest
          r@(Just{}) -> r

  goOverLines _ = Nothing

patternTest :: ScreenPattern -> T.Text -> Int -> (Int -> Int) -> Maybe Detailed

patternTest (Plain patterntxt) subject row indexer = case T.breakOn patterntxt subject of
  (leftside, rightside) | not (T.null rightside) ->
    Just $ Detailed { x = indexer (T.length leftside)
                    , y = row
                    , w = indexer (T.length leftside + T.length patterntxt) -
                          indexer (T.length leftside)
                    , h = 1
                    , matches = [T.take (T.length patterntxt) rightside] }

  _ -> Nothing

patternTest (RegexPattern regex) subject row indexer =
  case matchOnce regex (T.unpack subject) :: Maybe MatchArray of
    Nothing -> Nothing
    Just arr ->
      let lowest_offset = minimum $ fmap (\(offset, _) -> if offset /= -1 then offset else 1000000000) arr
          highest_offset = maximum $ fmap (\(offset, _) -> offset) arr
       in Just $ Detailed
            { x = indexer lowest_offset
            , y = row
            , w = indexer highest_offset - indexer lowest_offset
            , h = 1
            , matches = flip fmap (toList arr) $ \(offset, len) ->
                T.take len $ T.drop offset subject }

patternTest (RowLimited _ inner) subject row indexer = patternTest inner subject row indexer

-- | Convenience function to test regexes on ghci REPL.
--
-- @
--   debugPattern subject regex
-- @
debugPattern :: String -> String -> [MatchText String]
debugPattern subject reg =
  let r = makeRegex reg :: Regex
   in matchAllText r subject

