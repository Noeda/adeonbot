{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module Terminal.Screen
  ( textWidth
  , emptyScreenState
  , Cell(..)
  , ScreenColor(..)
  , intensify
  , getLine
  , getLine'
  , getCell
  , ScreenState()
  , ScreenStateMut()
  , screenSize
  , thawScreen
  , freezeScreen
  , writeCharacter
  , readCharacter
  , writeText
  , copy
  , ST()
  , runST
  , toANSIOutput )
  where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.Strict
import qualified Data.Array.IArray as A
import qualified Data.Array.ST as MA
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Data
import Data.Foldable
import Data.Hashable
import qualified Data.IntMap.Strict as IM
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Encoding as T
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics
import Prelude hiding ( getLine )
import System.Endian
import System.IO.Unsafe

foreign import ccall unsafe mk_wcswidth :: Ptr Word32 -> CSize -> IO CInt

textWidth :: T.Text -> Int
textWidth txt | T.null txt = 0
textWidth txt = unsafePerformIO $ do
  let txt32 = case getSystemEndianness of
                BigEndian -> T.encodeUtf32BE txt
                LittleEndian -> T.encodeUtf32LE txt
  B.unsafeUseAsCStringLen txt32 $ \(cstr, cstr_len) ->
    fromIntegral <$> mk_wcswidth
                     (castPtr cstr)
                     (fromIntegral cstr_len `div` fromIntegral (sizeOf (undefined :: Word32)))
{-# INLINE textWidth #-}

data ScreenColor
  = Black | Red | Green | Blue
  | Magenta | Cyan | Yellow | DarkGray
  | LightGray | BrightRed | BrightGreen | BrightBlue
  | BrightMagenta | BrightCyan | BrightYellow | White
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

instance Hashable ScreenColor

intensify :: ScreenColor -> ScreenColor
intensify Black = DarkGray
intensify Red = BrightRed
intensify Green = BrightGreen
intensify Blue = BrightBlue
intensify Magenta = BrightMagenta
intensify Cyan = BrightCyan
intensify Yellow = BrightYellow
intensify LightGray = White
intensify x = x

data Cell = Cell
  { foregroundColor :: !ScreenColor
  , backgroundColor :: !ScreenColor
  , contents        :: !T.Text }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

instance Hashable Cell

newtype ScreenState = ScreenState
  { rawCells :: A.Array (Int, Int) Cell }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

newtype ScreenStateMut s = ScreenStateMut
  { rawCellsMut :: MA.STArray s (Int, Int) Cell }
  deriving ( Typeable, Generic )

-- | Returns a line from screen state as text and also returning a function
-- that maps an index in that text to the column it is on the screen.
getLine :: Int -> ScreenState -> (T.Text, Int -> Int)
getLine row ss = getLine' row 0 ss

-- | Same as `getLine` but you can restrict to only start at column X.
--
-- @
--   getLine' row leftcolumn screenstate
-- @
getLine' :: Int -> Int -> ScreenState -> (T.Text, Int -> Int)
getLine' row leftcolumn ss@(ScreenState cells) =
  let (built, indexmap, _) = foldl' folder (mempty, IM.empty, 0) [leftcolumn..sw-1]
   in (TL.toStrict $ TB.toLazyText built
      ,\i -> snd $ fromMaybe (error "getLine: index out of range")
                             (IM.lookupLE i indexmap))
 where
  folder (builder, indexmap, txtindex) column =
    let cont = contents (cells A.! (column, row))
        part = TB.fromText cont
        new_indexmap = IM.insert txtindex column indexmap
        new_builder = builder <> part

     in new_builder `seq` new_indexmap `seq` (new_builder, new_indexmap, txtindex+T.length cont)

  (sw, sh) = screenSize ss

screenSize :: ScreenState -> (Int, Int)
screenSize (ScreenState { rawCells = rc }) =
  let ((0, 0), (actual_w, actual_h)) = A.bounds rc
   in (actual_w+1, actual_h+1)

copy :: ScreenStateMut s -> ScreenState -> (Int, Int) -> ST s ()
copy (ScreenStateMut { rawCellsMut = cells }) ss@(ScreenState { rawCells = src }) (ox, oy) = do
  let (sw, sh) = screenSize ss
  ((0, 0), (dw, dh)) <- MA.getBounds cells

  for_ [ (dx, dy) |
         dx <- [ox..min (ox+sw-1) dw],
         dy <- [oy..min (oy+sh-1) dh] ] $ \(dx, dy) -> do
    let (sx, sy) = (dx-ox, dy-oy)
    MA.writeArray cells (dx, dy) (src A.! (sx, sy))

emptyCell :: Cell
emptyCell = Cell { foregroundColor = White
                 , backgroundColor = Black
                 , contents = " " }

emptyScreenState :: Int -> Int -> ScreenState
emptyScreenState w h = ScreenState
  { rawCells = A.listArray
               ((0, 0), (actual_w, actual_h))
               (repeat emptyCell) }
 where
  actual_w = max 0 (w-1)
  actual_h = max 0 (h-1)

thawScreen :: ScreenState -> ST s (ScreenStateMut s)
thawScreen (ScreenState cells) =
  ScreenStateMut <$> MA.thaw cells
{-# INLINE thawScreen #-}

freezeScreen :: ScreenStateMut s -> ST s ScreenState
freezeScreen (ScreenStateMut cells) =
  ScreenState <$> MA.freeze cells
{-# INLINE freezeScreen #-}

writeText :: Int -> Int -> T.Text -> ScreenColor -> ScreenColor -> ScreenStateMut s -> ST s ()
writeText x y txt foreground background (ScreenStateMut cells) = do
  ((0, 0), (w, h)) <- MA.getBounds cells
  loop_it x y w h txt
 where
  loop_it _ _ _ _ txt | T.null txt = return ()
  loop_it x y w h txt = do
    let (pref, rest, sz) = takePrefix txt
    if x+sz <= w+1 && x >= 0 && y >= 0 && y <= h
      then do MA.writeArray cells (x, y) (Cell { contents = pref
                                               , foregroundColor = foreground
                                               , backgroundColor = background })
              loop_it (x+sz) y w h rest
      else return ()

takePrefix :: T.Text -> (T.Text, T.Text, Int)
takePrefix txt = loop_it T.empty txt
 where
  loop_it :: T.Text -> T.Text -> (T.Text, T.Text, Int)
  loop_it accum txt =
    let tw = textWidth accum
     in if tw > 0
          then (accum, txt, tw)
          else loop_it (accum <> T.take 1 txt) (T.drop 1 txt)
{-# INLINE takePrefix #-}

writeCharacter :: Int -> Int -> Cell -> ScreenStateMut s -> ST s ()
writeCharacter x y cell' (ScreenStateMut cells) = do
  ((0, 0), (w, h)) <- MA.getBounds cells
  when (x >= 0 && y >= 0 && y <= h && x+cellSize <= w+1) $
    MA.writeArray cells (x, y) cell
 where
  cellSize = textWidth (contents cell)
  cell = if T.null (contents cell')
           then cell' { contents = " " }
           else cell'
{-# INLINE writeCharacter #-}

readCharacter :: Int -> Int -> ScreenStateMut s -> ST s Cell
readCharacter x y (ScreenStateMut cells) = do
  ((0, 0), (w, h)) <- MA.getBounds cells
  if x >= 0 && y >= 0 && y <= h && x <= w
    then MA.readArray cells (x, y)
    else return $ Cell { foregroundColor = White
                       , backgroundColor = Black
                       , contents = " " }

foregroundCode :: ScreenColor -> B.ByteString
foregroundCode Black = "30"
foregroundCode Red = "31"
foregroundCode Green = "32"
foregroundCode Yellow = "33"
foregroundCode Blue = "34"
foregroundCode Magenta = "35"
foregroundCode Cyan = "36"
foregroundCode LightGray = "37"
foregroundCode DarkGray = "90"
foregroundCode BrightRed = "91"
foregroundCode BrightGreen = "92"
foregroundCode BrightYellow = "93"
foregroundCode BrightBlue = "94"
foregroundCode BrightMagenta = "95"
foregroundCode BrightCyan = "96"
foregroundCode White = "97"

backgroundCode :: ScreenColor -> B.ByteString
backgroundCode Black = "40"
backgroundCode Red = "41"
backgroundCode Green = "42"
backgroundCode Yellow = "43"
backgroundCode Blue = "44"
backgroundCode Magenta = "45"
backgroundCode Cyan = "46"
backgroundCode LightGray = "47"
backgroundCode DarkGray = "100"
backgroundCode BrightRed = "101"
backgroundCode BrightGreen = "102"
backgroundCode BrightYellow = "103"
backgroundCode BrightBlue = "104"
backgroundCode BrightMagenta = "105"
backgroundCode BrightCyan = "106"
backgroundCode White = "107"

-- | Given a screen state, and possibly and old state, returns a bytestring
-- that can be fed into an actual terminal and it should look like the
-- simulated terminal.
--
-- If you pass old screenstate, this will only write deltas.
toANSIOutput :: Maybe ScreenState -> ScreenState -> BL.ByteString
toANSIOutput old_ss ss@(ScreenState { rawCells = cells }) = BB.toLazyByteString $ execWriter $ do
  evalStateT (loop_rows 0) (True, White, Black, 0, 0)
 where
  (w, h) = screenSize ss

  full_refresh = case old_ss of
    Nothing -> True
    Just old_ss' -> screenSize old_ss' /= screenSize ss

  write bs = lift $ tell $ BB.byteString bs
  writeS str = lift $ tell $ BB.stringUtf8 str
  writeT txt = lift $ tell $ BB.byteString $ T.encodeUtf8 txt

  loop_rows y | y >= h = return ()
  loop_rows y = do
    loop_columns 0 y
    loop_rows (y+1)

  loop_columns x _ | x >= w = return ()
  loop_columns x y = do
    let c = cells A.! (x, y)
        cont = contents c
        w = textWidth cont

    case old_ss of
      Just old_ss' | full_refresh == False && rawCells old_ss' A.! (x, y) == c -> return ()
      _ -> do
        (first, f, b, cursorx, cursory) <- get
        when (first || f /= foregroundColor c || b /= backgroundColor c) $ do
          when first $ do
            write "\x1b[0m\x1b[1;1H"
            modify $ \(f, fcolor, bcolor, _, _) -> (f, fcolor, bcolor, 0, 0)

          modify $ \(_, _, _, cx, cy) -> (False, foregroundColor c, backgroundColor c, cx, cy)
          write $ "\x1b[" <> foregroundCode (foregroundColor c) <> ";" <> backgroundCode (backgroundColor c) <> "m"

        when (cursorx /= x || cursory /= y) $ do
          writeS $ "\x1b[" <> show (y+1) <> ";" <> show (x+1) <> "H"
          modify $ \(first, fground, bground, _, _) -> (first, fground, bground, x, y)

        writeT cont
        modify $ \(first, fground, bground, _, cursory) -> (first, fground, bground, x+w, cursory)

    loop_columns (x+w) y
{-# INLINEABLE toANSIOutput #-}

getCell :: Int -> Int -> ScreenState -> Cell
getCell x y ss@(ScreenState cells) =
  if x >= 0 && x <= sw-1 && y >= 0 && y <= sh-1
    then cells A.! (x, y)
    else Cell { contents = " "
              , foregroundColor = White
              , backgroundColor = Black }
 where
  (sw, sh) = screenSize ss

