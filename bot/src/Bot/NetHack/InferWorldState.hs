{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Bot.NetHack.InferWorldState
  ( inferWorldState )
  where

import Bot.NetHack.MonadAI
import Bot.NetHack.ScreenPattern
import Bot.NetHack.WorldState
import Control.Lens hiding ( Level, levels )
import Control.Monad.State.Strict
import qualified Data.Array.MArray as A
import qualified Data.Array.ST as A
import Data.Foldable
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as T
import Terminal.Screen

-- | Uses what is currently seen on the screen to infer and update bot's world
-- state (see `WorldState`).
inferWorldState :: MonadAI m => WorldState -> m WorldState
inferWorldState = execStateT $
  inferCurrentLevel

inferCurrentLevel :: MonadAI m => StateT WorldState m ()
inferCurrentLevel = do
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

inferCurrentlyStandingSquare :: MonadAI m => Level -> StateT WorldState m Level
inferCurrentlyStandingSquare lvl = do
  (_, cx, cy) <- currentScreen

  -- Check floor if
  -- 1) We don't know what level feature we are standing on.
  -- 2) There are items on the floor but we haven't checked which items
  -- exactly.
  let lcell = lvl^?cells.ix (cx, cy)
  if (isNothing $ join $ lcell^?_Just.cellFeature) ||
     (lcell^?_Just.cellItems == Just PileSeen)
    then checkFloor cx cy
    else return lvl
 where
  checkFloor cx cy = do
    send ":"
    thing <- matchf (limitRows [0] $ regex "There is (a |an )?([a-zA-Z]+|[a-zA-Z]+ [a-zA-Z]+) here.")
    what_is_that_thing <- case thing of
      [_, _, description] -> return $ descriptionToLevelFeature description
      _ -> do
        b <- matchf (limitRows [0] $ regex "You see no objects here.|You feel no objects here.|You see here|You feel here")
        return $ if b
          then Just Floor   -- This can also actually be a wall or rock if
                            -- you are embedded in it but it'll get fixed
                            -- when the player moves.
          else Nothing

    -- There might be a --More-- prompt here
    more <- matchf (limitRows [0, 1] "--More--")
    when more $ send " "

    -- Update floor if we got a positive match
    let update1 = case what_is_that_thing of
                    Nothing -> id
                    Just inferred_thing -> \x -> x & cells.ix (cx, cy).cellFeature .~ Just inferred_thing

    -- Do item list inferring
    newitems <- inferItemsBeingLookedAt (fromJust $ lvl^?cells.ix (cx, cy).cellItems)

    let update2 = \x -> x & cells.ix (cx, cy).cellItems .~ newitems

    return $ update2 $ update1 lvl

-- | Infers items on screen based on view you get by pressing :
--
-- May press space to look at more items if the pile has lots and lots.
inferItemsBeingLookedAt :: MonadAI m => ItemPileImage -> m ItemPileImage
inferItemsBeingLookedAt _ = do
  b <- matchf (limitRows [0] $ regex "You see no objects here.|You feel no objects here.")
  if b then return NoPile else inferListing
 where
  inferListing = do
    -- Single item case?
    matchf (limitRows [0] $ regex "You (see|feel) here (a |an |the )?(.+)( \\([0-9]+ aum\\))?\\.") >>= \case
      [_whole, _seefeel, _article, name, _weight] ->
        return $ Pile [nameToItem name]
      _ -> do
        -- Multi item case then
        matchf (limitRows [0] $ regex "Things that you feel here:|Things that you see here:") >>= \case
          Just dd -> itemInferLoop dd
          Nothing ->
            return NoPile

  itemInferLoop dd = do
    (ss, _, _) <- currentScreen

    let left_column = x dd
        -- Row where item listing starts (immediately below header)
        top_row = y dd + 1

    -- There should be a --More-- at the bottom
    matchf "--More--" >>= \case
      Just more_dd -> do
        let bottom_row = y more_dd - 1
            item_lines = [ T.strip (fst (getLine' row left_column ss)) |
                           row <- [top_row..bottom_row] ]
            items = fmap nameToItem item_lines

        send " "

        -- If there is another --More--, parse more items
        b <- matchf "--More--"
        if b
          then do rest_of_items <- itemInferLoop (dd { x = 0
                                                     , y = -1 })
                  case rest_of_items of
                    Pile items2 -> return $ Pile $ items <> items2
                    _ -> return $ Pile items
          else return $ Pile items

      Nothing ->
        -- This shouldn't happen. But maybe it did anyway?
        return NoPile

nameToItem :: T.Text -> Item
nameToItem = error . show

descriptionToLevelFeature :: T.Text -> Maybe LevelFeature
descriptionToLevelFeature "staircase up" = Just Upstairs
descriptionToLevelFeature "staircase down" = Just Downstairs
descriptionToLevelFeature "fountain" = Just Fountain
descriptionToLevelFeature "doorway" = Just Floor
descriptionToLevelFeature "broken door" = Just Floor
descriptionToLevelFeature "open door" = Just OpenedDoor
descriptionToLevelFeature "molten lava" = Just Lava
descriptionToLevelFeature "pool of water" = Just Water
descriptionToLevelFeature "opulent throne" = Just Floor
descriptionToLevelFeature "tree" = Just Wall
descriptionToLevelFeature "hole" = Just Trap
descriptionToLevelFeature "pit" = Just Trap
descriptionToLevelFeature "web" = Just Trap
descriptionToLevelFeature "ladder down" = Just Downstairs
descriptionToLevelFeature "ladder up" = Just Upstairs
descriptionToLevelFeature "grave" = Just Floor
descriptionToLevelFeature "sink" = Just Floor

descriptionToLevelFeature txt | T.isInfixOf "altar" txt = Just Altar
descriptionToLevelFeature txt | T.isInfixOf "trap" txt = Just Trap
descriptionToLevelFeature _ = Nothing

isItemSymbol :: T.Text -> ScreenColor -> Bool
isItemSymbol "(" _ = True
isItemSymbol ")" _ = True
isItemSymbol "\"" fcolor | fcolor == Cyan = True
isItemSymbol "!" _ = True
isItemSymbol "?" _ = True
isItemSymbol "/" _ = True
isItemSymbol "=" _ = True
isItemSymbol "+" _ = True
isItemSymbol "*" _ = True
isItemSymbol "`" _ = True
isItemSymbol "$" _ = True
isItemSymbol "%" _ = True
isItemSymbol "0" fcolor | fcolor == Cyan = True
isItemSymbol "_" fcolor | fcolor == Cyan = True
isItemSymbol _ _ = False

inferLevel :: MonadAI m => Level -> StateT WorldState m Level
inferLevel lvl = do
  (ss, cx, cy) <- currentScreen
  statuses <- use statuses
  let (sw, sh) = screenSize ss

      new_cells = A.runSTArray $ do
                    mutcells <- A.thaw (lvl^.cells)
                    inferring cx cy sw sh statuses mutcells ss
                    return mutcells

      new_boulders = inferBoulders ss

  let updated_lvl = lvl & (cells .~ new_cells) .
                          (boulders .~ new_boulders)

  inferCurrentlyStandingSquare updated_lvl
 where
  inferBoulders :: ScreenState -> S.Set (Int, Int)
  inferBoulders ss = foldl' folding S.empty [(x, y) | x <- [0..sw-1], y <- [1..sh-3]]
   where
    (sw, sh) = screenSize ss

    folding set (x, y) =
      let cell = getCell x y ss
       in if contents cell == "0" && foregroundColor cell == LightGray
            then S.insert (x, y) set
            else set

  inferring :: Int
            -> Int
            -> Int
            -> Int
            -> S.Set Status
            -> A.STArray s (Int, Int) LevelCell
            -> ScreenState
            -> ST s ()
  inferring cx cy sw sh statuses mutcells ss = for_ [0..sw-1] $ \column -> for_ [1..sh-3] $ \row ->
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
            "_" | fcolor == LightGray -> Just Altar
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
        Nothing ->
          -- If we can't infer any new information then maybe we can infer
          -- something about items?
          when (isItemSymbol (contents cell) (foregroundColor cell)) $ do
            old_cell <- A.readArray mutcells (column, row)
            let scell = show cell

            -- Mark item pile dirty if its appearance has changed from last
            -- time.
            when ((old_cell^.cellItemAppearanceLastTime) /= scell) $
              A.writeArray mutcells (column, row)
                (old_cell & (cellItems .~ PileSeen) .
                            (cellItemAppearanceLastTime .~ scell))

        Just new_feature -> do
          old_cell <- A.readArray mutcells (column, row)
          A.writeArray mutcells (column, row)
            (old_cell & (cellFeature .~ Just new_feature) .
                        (cellItemAppearanceLastTime .~ "") .  -- Also clear item pile
                        (cellItems .~ NoPile))

