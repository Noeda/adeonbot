{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Bot.NetHack.InferWorldState
  ( inferWorldState )
  where

import Bot.NetHack.MonadAI
import Bot.NetHack.WorldState
import Control.Lens hiding ( Level, levels )
import Control.Monad.State.Strict
import qualified Data.Array.IArray as A
import qualified Data.Array.MArray as A
import qualified Data.Array.ST as A
import Data.Foldable
import qualified Data.Set as S
import Terminal.Screen

-- | Uses what is currently seen on the screen to infer and update bot's world
-- state (see `WorldState`).
inferWorldState :: MonadAI m => WorldState -> m WorldState
inferWorldState = execStateT $
  inferCurrentLevel

inferCurrentLevel :: MonadAI m => StateT WorldState m ()
inferCurrentLevel = do
  (st, _, _) <- currentScreen

  -- Does the level we are currently on "exist" (i.e. have we seen it before)?
  cl <- use currentLevel
  use (levels.at cl) >>= \case
    Nothing -> do
      freshLevel cl  -- make a new fresh level, then try again
      inferCurrentLevel
    Just lvl -> do
      updated_lvl <- inferLevel lvl
      levels.at cl .= Just updated_lvl

freshLevel :: MonadAI m => LevelIndex -> StateT WorldState m ()
freshLevel li = levels.at li .= Just emptyLevel

inferLevel :: MonadAI m => Level -> StateT WorldState m Level
inferLevel lvl = do
  (ss, cx, cy) <- currentScreen
  statuses <- use statuses
  let (sw, sh) = screenSize ss

      new_cells = A.runSTArray $ do
                    mutcells <- A.thaw (lvl^.cells)
                    inferring cx cy sw sh statuses mutcells ss
                    return mutcells

  return $ lvl & cells .~ new_cells
 where
  inferring :: Int
            -> Int
            -> Int
            -> Int
            -> S.Set Status
            -> A.STArray s (Int, Int) LevelCell
            -> ScreenState
            -> ST s ()
  inferring cx cy sw sh statuses mutcells ss = for_ [0..sw-1] $ \column -> for_ [0..sh-3] $ \row ->
    -- Don't infer anything at the spot player is standing
    unless (column == cx && row == cy) $ do
      let can_infer_stuff_next_to_player = (Blind `S.notMember` statuses) &&
                                           (abs (cx-column) <= 1 && abs (cy-row) <= 1)
          cell = getCell column row ss
          fcolor = foregroundColor cell

          -- Here we infer stuff just by how they look like
          -- Return Just if there is no question this is the thing.
          -- Return Nothing if you don't want to modify existing feature.
          inferred = case contents cell of
            "." -> Just Floor
            "_" -> Just Altar
            "#" | fcolor == Green -> Just Wall
            "#" | fcolor == Cyan -> Just Wall
            "#" | fcolor == Yellow -> Just Wall
            "#" -> Just Floor
            "~" | fcolor == LightGray -> Just Floor
            "~" | fcolor == Yellow -> Just ClosedDoor
            "-" | fcolor == Yellow -> Just OpenedDoor
            "|" | fcolor == Yellow -> Just OpenedDoor
            "-" | fcolor == LightGray -> Just Wall
            "|" | fcolor == LightGray -> Just Wall
            "^" -> Just Trap
            "\"" | fcolor == LightGray -> Just Trap
            ">" -> Just Downstairs
            "<" -> Just Upstairs
            "\\" | fcolor == BrightYellow -> Just Floor
            "{" | fcolor == Blue -> Just Fountain
            "}" | fcolor == Blue -> Just Water
            "}" | fcolor == Red -> Just Lava
            " " | can_infer_stuff_next_to_player -> Just Wall
            _ -> Nothing

      case inferred of
        Nothing -> return ()
        Just new_feature -> do
          old_cell <- A.readArray mutcells (column, row)
          A.writeArray mutcells (column, row) (old_cell & cellFeature .~ Just new_feature)

