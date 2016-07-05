{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Terminal.Emulator
  ( EmulatorF(..)
  , Emulator
  , emulator
  , module Control.Monad.Free )
  where

import qualified Data.ByteString as B
import Control.Monad.Free
import Control.Monad.State.Strict
import Data.Data
import Data.Foldable
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word
import GHC.Generics
import Terminal.Screen

data EmulatorF f
  = ReadByte (Word8 -> f)
  | PeekByte (Word8 -> f)
  | YieldChange (forall s. ScreenStateMut s -> ST s ()) f
  | GetCurrentState (ScreenState -> f)
  deriving ( Functor, Typeable )

type Emulator a = Free EmulatorF a

class Monad m => MonadEmulator m where
  readByte :: m Word8
  peekByte :: m Word8
  yield :: (forall s. ScreenStateMut s -> ST s ()) -> m ()
  getScreenState :: m ScreenState

instance MonadEmulator (Free EmulatorF) where
  readByte = liftF $ ReadByte id
  peekByte = liftF $ PeekByte id
  yield fun = liftF $ YieldChange fun ()
  getScreenState = liftF $ GetCurrentState id

instance MonadEmulator m => MonadEmulator (StateT s m) where
  readByte = lift readByte
  peekByte = lift peekByte
  yield fun = lift $ yield fun
  getScreenState = lift getScreenState

data EmulatorState = EmulatorState
  { cursor :: !(Int, Int)
  , currentForegroundColor :: !ScreenColor
  , currentBackgroundColor :: !ScreenColor
  , showCursor :: !Bool
  , bold :: !Bool
  , inverse :: !Bool }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

getActualColor :: EmulatorState -> (ScreenColor, ScreenColor)
getActualColor es | bold es == True =
  getActualColor (es { bold = False
                     , currentForegroundColor = intensify $ currentForegroundColor es
                     , currentBackgroundColor = currentBackgroundColor es } )
getActualColor es | inverse es == True =
  getActualColor (es { inverse = False
                     , currentForegroundColor = currentBackgroundColor es
                     , currentBackgroundColor = currentForegroundColor es })
getActualColor es = (currentForegroundColor es, currentBackgroundColor es)

initialEmulatorState :: EmulatorState
initialEmulatorState = EmulatorState
  { cursor = (0, 0)
  , currentForegroundColor = LightGray
  , currentBackgroundColor = Black
  , showCursor = True
  , bold = False
  , inverse = False }

-- | Reads semicolon separated integers out of terminal.
readNumbers :: MonadEmulator m => m [Int]
readNumbers = do
  b <- peekByte

  if | isDigit b -> void readByte >> consumeDigit (fromIntegral b-48)
     | otherwise -> return []
 where
  isDigit x = x >= 48 && x <= 57

  consumeDigit !accumulator = do
    b <- peekByte
    if | isDigit b -> void readByte >> consumeDigit (accumulator*10 + (fromIntegral b-48))
       -- semicolon?
       | b == 59 -> do void readByte
                       (accumulator:) <$> readNumbers
       | otherwise -> return [accumulator]

-- | Reads a glyph from input.
--
-- This reads until full code point has been removed (it deals correctly with
-- utf-8 boundaries, not returning until full code point has been returned) and
-- it also blocks until the text would occupy more than 0 columns in a terminal
-- (so combining characters are returned with the payload).
readGlyph :: MonadEmulator m => m T.Text
readGlyph = do
  b <- readByte
  loop_it (B.singleton b)
 where
  loop_it accum = case T.streamDecodeUtf8With (\_ _ -> Just '?') accum of
    -- No full unicode code point yet (null txt).
    T.Some txt _ _ | T.null txt -> do
      next_byte <- readByte
      loop_it (accum <> B.singleton next_byte)

    -- A complete code point has been read. But is it more than 0 characters
    -- wide?
    T.Some txt leftovers _ | (textWidth txt > 0 ||
                              T.any (\x -> x < ' ') txt) &&
                             (B.null leftovers) -> return txt

    _ -> do
      next_byte <- readByte
      loop_it (accum <> B.singleton next_byte)

{-# INLINEABLE readGlyph #-}

getScreenSize :: MonadEmulator m => m (Int, Int)
getScreenSize = do
  st <- getScreenState
  return $ screenSize st

handleQuestionSequence :: MonadEmulator m => [Int] -> T.Text -> StateT EmulatorState m ()

-- DECRST
handleQuestionSequence numbers "l" = for_ numbers $ \case
  25 -> modify $ \old -> old { showCursor = False }
  _ -> return ()

-- DECSET
handleQuestionSequence numbers "h" = for_ numbers $ \case
  25 -> modify $ \old -> old { showCursor = True }
  _ -> return ()

handleQuestionSequence _ _ = return ()

handleEscapeSequence :: MonadEmulator m => [Int] -> T.Text -> StateT EmulatorState m ()

-- Reset Mode (RM)
handleEscapeSequence _ "l" = return ()
-- Set Mode (SM)
handleEscapeSequence _ "h" = return ()

-- Cursor backward
handleEscapeSequence [] "D" = modify $ \old -> old { cursor = (fst (cursor old) - 1, snd $ cursor old) }
handleEscapeSequence [x] "D" = modify $ \old -> old { cursor = (max 0 $ fst (cursor old) - x, snd $ cursor old) }
handleEscapeSequence _ "D" = return ()

-- Cursor forward
handleEscapeSequence [] "C" = handleEscapeSequence [1] "C"
handleEscapeSequence [x] "C" = do
  (sw, _) <- getScreenSize
  modify $ \old -> old { cursor = (min (sw-1) $ fst (cursor old) + x, snd $ cursor old) }
handleEscapeSequence _ "C" = return ()

-- Cursor up
handleEscapeSequence [] "A" = handleEscapeSequence [1] "A"
handleEscapeSequence [x] "A" = do
  modify $ \old -> old { cursor = (fst $ cursor old, max 0 $ snd (cursor old) - x) }
handleEscapeSequence _ "A" = return ()

-- Cursor down
handleEscapeSequence [] "B" = handleEscapeSequence [1] "B"
handleEscapeSequence [x] "B" = do
  (_, sh) <- getScreenSize
  modify $ \old -> old { cursor = (fst $ cursor old, min (sh-1) $ snd (cursor old) + x) }
handleEscapeSequence _ "B" = return ()


-- Line position (absolute) row
handleEscapeSequence [] "d" = modify $ \old -> old { cursor = (fst $ cursor old, 0) }
handleEscapeSequence [row] "d" = modify $ \old -> old { cursor = (fst $ cursor old, row-1) }
handleEscapeSequence _ "d" = return ()

-- Set cursor position
handleEscapeSequence [] "H" = modify $ \old -> old { cursor = (0, 0) }
handleEscapeSequence [row] "H" = modify $ \old -> old { cursor = (fst $ cursor old, row-1) }
handleEscapeSequence [row,column] "H" = modify $ \old -> old { cursor = (column-1, row-1) }
handleEscapeSequence _ "H" = return ()

-- Erase in line
handleEscapeSequence numbers "K" =
  if null numbers
    then eraseRight
    else for_ numbers $ \case
           0 -> eraseRight
           1 -> eraseLeft
           2 -> eraseAll
           _ -> return ()
 where
  eraseRight = do
    (cx, cy) <- cursor <$> get
    (sw, _) <- getScreenSize
    (fcolor, bcolor) <- getActualColor <$> get
    yield $ \mut ->
      for_ [cx..sw-1] $ \column ->
        writeCharacter column cy (Cell { foregroundColor = fcolor
                                       , backgroundColor = bcolor
                                       , contents = " " }) mut

  eraseLeft = do
    (cx, cy) <- cursor <$> get
    (fcolor, bcolor) <- getActualColor <$> get
    yield $ \mut ->
      for_ [cx-1,cx-2..0] $ \column ->
        writeCharacter column cy (Cell { foregroundColor = fcolor
                                       , backgroundColor = bcolor
                                       , contents = " " }) mut

  eraseAll = do
    (_, cy) <- cursor <$> get
    (sw, _) <- getScreenSize
    (fcolor, bcolor) <- getActualColor <$> get
    yield $ \mut ->
      for_ [0..sw-1] $ \column ->
        writeCharacter column cy (Cell { foregroundColor = fcolor
                                       , backgroundColor = bcolor
                                       , contents = " " }) mut

-- Clear display
handleEscapeSequence numbers "J" =
  if null numbers
    then eraseBelow
    else for_ numbers $ \case
           0 -> eraseBelow
           1 -> eraseAbove
           2 -> eraseAll
           _ -> return ()
 where
  eraseAll = do
    (sw, sh) <- getScreenSize
    (fcolor, bcolor) <- getActualColor <$> get
    yield $ \mut ->
      for_ [0..sw-1] $ \column ->
        for_ [0..sh-1] $ \row ->
          writeCharacter column row (Cell { foregroundColor = fcolor
                                          , backgroundColor = bcolor
                                          , contents = " " }) mut

  eraseAbove = do
    (_, cy) <- cursor <$> get
    (sw, _) <- getScreenSize
    (fcolor, bcolor) <- getActualColor <$> get
    yield $ \mut ->
      for_ [0..sw-1] $ \column ->
        for_ [cy-1,cy-2..0] $ \row ->
          writeCharacter column row (Cell { foregroundColor = fcolor
                                          , backgroundColor = bcolor
                                          , contents = " " }) mut

  eraseBelow = do
    (_, cy) <- cursor <$> get
    (sw, sh) <- getScreenSize
    (fcolor, bcolor) <- getActualColor <$> get
    yield $ \mut ->
      for_ [0..sw-1] $ \column ->
        for_ [cy+1..sh-1] $ \row ->
          writeCharacter column row (Cell { foregroundColor = fcolor
                                          , backgroundColor = bcolor
                                          , contents = " " }) mut

-- Attribute change (e.g.  \x1b[5m )
handleEscapeSequence [] "m" =
  modify $ \old -> old { currentForegroundColor = LightGray
                            , currentBackgroundColor = Black
                            , bold = False
                            , inverse = False }
handleEscapeSequence numbers "m" = for_ numbers $ \case
  0 -> modify $ \old -> old { currentForegroundColor = LightGray
                            , currentBackgroundColor = Black
                            , bold = False
                            , inverse = False }
  1 -> modify $ \old -> old { bold  = True }
  7 -> modify $ \old -> old { inverse = True }
  22 -> modify $ \old -> old { bold  = False }
  27 -> modify $ \old -> old { inverse = False }

  30 -> modify $ \old -> old { currentForegroundColor = Black }
  31 -> modify $ \old -> old { currentForegroundColor = Red }
  32 -> modify $ \old -> old { currentForegroundColor = Green }
  33 -> modify $ \old -> old { currentForegroundColor = Yellow }
  34 -> modify $ \old -> old { currentForegroundColor = Blue }
  35 -> modify $ \old -> old { currentForegroundColor = Magenta }
  36 -> modify $ \old -> old { currentForegroundColor = Cyan }
  37 -> modify $ \old -> old { currentForegroundColor = LightGray }
  39 -> modify $ \old -> old { currentForegroundColor = LightGray }

  40 -> modify $ \old -> old { currentBackgroundColor = Black }
  41 -> modify $ \old -> old { currentBackgroundColor = Red }
  42 -> modify $ \old -> old { currentBackgroundColor = Green }
  43 -> modify $ \old -> old { currentBackgroundColor = Yellow }
  44 -> modify $ \old -> old { currentBackgroundColor = Blue }
  45 -> modify $ \old -> old { currentBackgroundColor = Magenta }
  46 -> modify $ \old -> old { currentBackgroundColor = Cyan }
  47 -> modify $ \old -> old { currentBackgroundColor = LightGray }
  49 -> modify $ \old -> old { currentBackgroundColor = Black }

  90 -> modify $ \old -> old { currentForegroundColor = DarkGray }
  91 -> modify $ \old -> old { currentForegroundColor = BrightRed }
  92 -> modify $ \old -> old { currentForegroundColor = BrightGreen }
  93 -> modify $ \old -> old { currentForegroundColor = BrightYellow }
  94 -> modify $ \old -> old { currentForegroundColor = BrightBlue }
  95 -> modify $ \old -> old { currentForegroundColor = BrightMagenta }
  96 -> modify $ \old -> old { currentForegroundColor = BrightCyan }
  97 -> modify $ \old -> old { currentForegroundColor = White }

  100 -> modify $ \old -> old { currentForegroundColor = DarkGray }
  101 -> modify $ \old -> old { currentForegroundColor = BrightRed }
  102 -> modify $ \old -> old { currentForegroundColor = BrightGreen }
  103 -> modify $ \old -> old { currentForegroundColor = BrightYellow }
  104 -> modify $ \old -> old { currentForegroundColor = BrightBlue }
  105 -> modify $ \old -> old { currentForegroundColor = BrightMagenta }
  106 -> modify $ \old -> old { currentForegroundColor = BrightCyan }
  107 -> modify $ \old -> old { currentForegroundColor = White }

  _ -> return ()
handleEscapeSequence _ _ = return ()

emulator :: Emulator ()
emulator = flip evalStateT initialEmulatorState $ forever $ do
  -- Flip cursor pos
  st <- get

  when (showCursor st) $ do
    (cx, cy) <- cursor <$> get
    yield $ \mut -> do
      ch <- readCharacter cx cy mut
      let ch' = ch { foregroundColor = backgroundColor ch
                   , backgroundColor = foregroundColor ch }
      writeCharacter cx cy ch' mut

  ch <- readGlyph

  -- Flip cursor back
  when (showCursor st) $ do
    (cx, cy) <- cursor <$> get
    yield $ \mut -> do
      ch <- readCharacter cx cy mut
      let ch' = ch { foregroundColor = backgroundColor ch
                   , backgroundColor = foregroundColor ch }
      writeCharacter cx cy ch' mut

  if | T.length ch /= 1 || T.head ch >= ' ' -> yieldChar ch
     | T.length ch == 1 ->
         if | T.head ch == '\x1b' -> escapeSequence
            | T.head ch == '\n' -> nextLine
            | T.head ch == '\r' -> carriageReturn
            | T.head ch == '\b' -> backspace
            | T.head ch == '\t' -> tab
            | otherwise -> return ()
     | otherwise -> return ()
 where
  goToNextLine = do
    (_, cy) <- cursor <$> get
    (_, sh) <- getScreenSize
    if cy == sh-1
      then scrollUp >> goToNextLine
      else modify $ \old -> old { cursor = (0, cy+1) }

  backspace = do
    (cx, cy) <- cursor <$> get
    when (cx > 0) $
      modify $ \old -> old { cursor = (cx-1, cy) }

  tab = do
    (cx, cy) <- cursor <$> get
    (sw, _) <- getScreenSize
    let cx8 = cx `mod` 8
        new_cx = min (sw-1) $ if cx8 /= 0
                   then cx + (8 - cx8)
                   else cx + 8

    modify $ \old -> old { cursor = (new_cx, cy) }

  nextLine = do
    (cx, cy) <- cursor <$> get
    (_, sh) <- getScreenSize
    if cy == sh-1
      then scrollUp >> nextLine
      else modify $ \old -> old { cursor = (cx, cy+1) }

  carriageReturn = do
    (_, cy) <- cursor <$> get
    modify $ \old -> old { cursor = (0, cy) }

  escapeSequence = do
    ch <- readGlyph
    if | ch == "[" -> do
           b <- peekByte
           handler <- if b == 63   -- question mark
             then void readByte >> return handleQuestionSequence
             else return handleEscapeSequence
           numbers <- readNumbers
           control_ch <- readGlyph
           handler numbers control_ch
       | ch == "(" -> do -- designate charset
           void readByte
       | otherwise -> return ()

  scrollUp = do
    (cx, cy) <- cursor <$> get
    (fcolor, bcolor) <- getActualColor <$> get

    (sw, sh) <- getScreenSize
    modify $ \old -> old { cursor = (cx, cy-1) }

    yield $ \mut -> do
      for_ [0..sh-2] $ \row ->
        for_ [0..sw-1] $ \column -> do
          ch <- readCharacter column (row+1) mut
          writeCharacter column row ch mut

      for_ [0..sw-1] $ \column -> do
        writeCharacter column (sh-1) (Cell { foregroundColor = fcolor
                                           , backgroundColor = bcolor
                                           , contents = " " }) mut

  yieldChar ch = do
    let w = textWidth ch
    (sw, _) <- getScreenSize
    (cx, _) <- cursor <$> get

    -- Would the character fit in our terminal in the first place?
    -- (Think extremely thin terminals)
    if sw < w
      then return () -- don't do anything
      else -- Would the character fit where we are?
           (if cx+w > sw
              -- nope, go to next line
              then goToNextLine >> yieldChar ch
              else putCharRightHere ch)
   where
     putCharRightHere ch = do
       (cx, cy) <- cursor <$> get
       (fcolor, bcolor) <- getActualColor <$> get
       let w = textWidth ch
       yield $ \mut -> do
         writeCharacter cx cy (Cell { foregroundColor = fcolor
                                    , backgroundColor = bcolor
                                    , contents        = ch })
                              mut
       modify $ \old -> old { cursor = (cx+w, cy) }

