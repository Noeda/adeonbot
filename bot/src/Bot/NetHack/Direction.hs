{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}

module Bot.NetHack.Direction
  ( Direction(..)
  , directionToLetter
  , directionToByteString
  , diffToDir
  , diffToSend
  , birdMovementKeysTo
  , neighboursOf
  , movePosByDir )
  where

import Data.Aeson
import qualified Data.ByteString as B
import Data.Char
import Data.Data
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word
import GHC.Generics

data Direction
  = D7
  | D8
  | D9
  | D4
  | D6
  | D1
  | D2
  | D3
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum, FromJSON, ToJSON, FromJSONKey, ToJSONKey )

directionToLetter :: Direction -> Char
directionToLetter D8 = 'k'
directionToLetter D2 = 'j'
directionToLetter D4 = 'h'
directionToLetter D6 = 'l'
directionToLetter D7 = 'y'
directionToLetter D9 = 'u'
directionToLetter D1 = 'b'
directionToLetter D3 = 'n'

directionToByteString :: Direction -> B.ByteString
directionToByteString dir = T.encodeUtf8 $ T.pack [directionToLetter dir]

movePosByDir :: (Int, Int) -> Direction -> (Int, Int)
movePosByDir (x, y) D8 = (x, y-1)
movePosByDir (x, y) D2 = (x, y+1)
movePosByDir (x, y) D4 = (x-1, y)
movePosByDir (x, y) D6 = (x+1, y)
movePosByDir (x, y) D1 = (x-1, y+1)
movePosByDir (x, y) D3 = (x+1, y+1)
movePosByDir (x, y) D7 = (x-1, y-1)
movePosByDir (x, y) D9 = (x+1, y-1)

diffToDir :: (Int, Int) -> (Int, Int) -> Maybe Direction
diffToDir (x1, y1) (x2, y2) =
  if | x1 == x2 && y1 == y2-1 -> Just D2
     | x1 == x2 && y1 == y2+1 -> Just D8
     | x1 == x2-1 && y1 == y2 -> Just D6
     | x1 == x2+1 && y1 == y2 -> Just D4
     | x1 == x2-1 && y1 == y2-1 -> Just D3
     | x1 == x2+1 && y1 == y2-1 -> Just D1
     | x1 == x2-1 && y1 == y2+1 -> Just D9
     | x1 == x2+1 && y1 == y2+1 -> Just D7
     | otherwise -> Nothing

diffToSend :: (Int, Int) -> (Int, Int) -> Maybe B.ByteString
diffToSend pos1 pos2 =
  directionToByteString <$> diffToDir pos1 pos2

neighboursOf :: Int -> Int -> [(Int, Int)]
neighboursOf x y =
  [(nx, ny) | nx <- [x-1..x+1], ny <- [y-1..y+1], nx /= x || ny /= y]

-- This functions returns the cardinal direction keys to go from point A to
-- point B.
--
-- E.g. (1, 1) -> (5, 5) returns "lllljjjj".
birdMovementKeysTo :: (Int, Int) -> (Int, Int) -> B.ByteString
birdMovementKeysTo (cx, cy) (tx, ty) =
  let gorights = max 0 (tx-cx)
      golefts  = max 0 (cx-tx)
      goups    = max 0 (cy-ty)
      godowns  = max 0 (ty-cy)

    in B.replicate gorights (b8 'l') <>
       B.replicate golefts (b8 'h') <>
       B.replicate goups (b8 'k') <>
       B.replicate godowns (b8 'j')

b8 :: Char -> Word8
b8 x =
  let o = ord x
   in if o > 255 || o < 0
        then error "b8: char cannot be turned into byte."
        else fromIntegral o

