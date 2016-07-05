{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.NetHack.ScreenPattern
  ( ScreenPattern()
  , Match(..)
  , Detailed(..)
  , match )
  where

import Data.Data
import Data.String
import qualified Data.Text as T
import GHC.Generics
import Prelude hiding ( getLine )
import Terminal.Screen

newtype ScreenPattern = ScreenPattern T.Text

instance IsString ScreenPattern where
  fromString str = ScreenPattern $ T.pack str

data Detailed = Detailed
  { x :: !Int
  , y :: !Int
  , w :: !Int
  , h :: !Int }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

class Match match where
  fromDetailed :: Maybe Detailed -> match

instance Match Bool where
  fromDetailed (Just{}) = True
  fromDetailed Nothing = False

match :: Match match => ScreenState -> ScreenPattern -> match
match ss (ScreenPattern patterntxt) = fromDetailed $ goOverLines 0
 where
  (_, sh) = screenSize ss

  goOverLines row | row < sh =
    let (txt, indexer) = getLine row ss
     in case T.breakOn patterntxt txt of
          (leftside, rightside) | not (T.null rightside) ->
            Just $ Detailed { x = indexer (T.length leftside)
                            , y = row
                            , w = indexer (T.length leftside + T.length patterntxt) -
                                  indexer (T.length leftside)
                            , h = 1 }

          _ -> goOverLines (row+1)

  goOverLines _ = Nothing

