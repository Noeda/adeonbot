{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Bot.NetHack.InferWorldState
  ( inferWorldState
  , inferSquare
  , inferSearch
  , inferPeaceful
  , dirtyInventory
  , setLastSearchedPosition
  , nameToItem
  , levelSquares )
  where

import Bot.NetHack.MonadAI
import Bot.NetHack.ScreenPattern
import Bot.NetHack.WorldState
import Control.Lens hiding ( Level, levels )
import Control.Monad.State.Strict
import qualified Data.Array.MArray as A
import qualified Data.Array.ST as A
import Data.Foldable
import qualified Data.IntMap.Strict as IM
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as T
import Text.Regex.TDFA hiding ( match )
import Data.Traversable ( for )
import Terminal.Screen

-- | Uses what is currently seen on the screen to infer and update bot's world
-- state (see `WorldState`).
inferWorldState :: MonadAI m => [T.Text] -> WorldState -> m WorldState
inferWorldState messages = execStateT $ do
  inferStatuses
  inferCurrentLevel messages

inferLevelIndex :: MonadAI m => StateT WorldState m ()
inferLevelIndex = do
  cl <- use currentLevel
  expected_description <- use (levelMeta.at cl._Just.levelDescription)

  last_line <- getScreenLine 23

  let lvl_description = T.strip $ fst $ T.breakOn "$:" last_line
  if lvl_description == expected_description
    then return ()
    else do send "#overview\n"
            lookForYouAreHere lvl_description
 where
  lookForYouAreHere lvl_description = do
    -- Use the --More-- prompt to figure the column where branches are
    -- listed.
    Just dd <- matchf "--More--"

    let branch_column = if x dd == 1
                          then 0
                          else x dd

        branchFolder branchmap row = do line <- getScreenLine' row branch_column
                                        return $ if T.head line /= ' '
                                          then M.insert row (T.strip line) branchmap
                                          else branchmap

    -- We don't actually look at the level names at all. Just branch
    -- names and where "You are here" sign is.
    -- This builds a map of branch names on the screen, indexed by row
    branch_names <- foldlM branchFolder M.empty [0..y dd-1]

    -- Look for the "You are here" sign.
    matchf "<- You are here." >>= \case
      -- Maybe on next page?
      Nothing -> do send " "
                    lookForYouAreHere lvl_description
      Just you_are_here_row -> do
        let Just (_, branchname) = M.lookupLE (y you_are_here_row) branch_names

        -- Do we know this level?
        levelmetas <- use levelMeta

        let looper [] = return False
            looper ((lvlindex, meta):rest) = if meta^.levelDescription == lvl_description &&
                                                meta^.branchName == branchname
                                               then do currentLevel .= lvlindex
                                                       return True
                                               else looper rest

        found_level <- looper (IM.assocs levelmetas)
        unless found_level $ do
          let new_index = if IM.null levelmetas
                            then 1
                            else let (largest_key, _) = IM.findMax levelmetas
                                  in largest_key+1

          levelMeta.at new_index .= Just (LevelMeta { _levelDescription = lvl_description
                                                    , _branchName = branchname })
          levels.at new_index .= Just emptyLevel
          currentLevel .= new_index

        -- Dismiss --More--s
        dismissMores

  dismissMores = do
    b <- matchf "--More--"
    when b $ do
      send " "
      dismissMores

inferStatuses :: MonadAI m => StateT WorldState m ()
inferStatuses = do
  matchf (regex "T:[0-9]+ +(.+)$") >>= \case
    [_whole, T.strip -> status_line] -> do
      let status_words = fmap T.strip $ T.words status_line
      statuses .= S.empty
      for_ status_words $ \case
        "Blind" -> statuses %= S.insert Blind
        "FoodPois" -> statuses %= S.insert FoodPoisoning
        "Conf" -> statuses %= S.insert Confstunned
        "Stun" -> statuses %= S.insert Confstunned
        "Hungry" -> statuses %= S.insert Hungry
        "Weak" -> statuses %= S.insert Hungry
        "Fainting" -> statuses %= S.insert Hungry
        _ -> return ()
    _ -> return ()

inferInventory :: MonadAI m => [T.Text] -> StateT WorldState m ()
inferInventory messages = do
  let msgcheck m = any (T.isInfixOf m) messages

  when (msgcheck "boils and explodes" ||
        msgcheck "freezes and shatters" ||
        msgcheck "catches fire and burns" ||
        msgcheck "turns to dust" ||
        msgcheck "breaks apart and" ||
        msgcheck "steals" ||
        msgcheck "turn into" ||
        msgcheck "attracted to" ||
        msgcheck "stands still while" ||
        msgcheck "no gold" ||
        msgcheck "zap") $
    inventoryDirty .= True

  inventory_is_dirty <- use inventoryDirty
  when inventory_is_dirty $ do
    inventoryDirty .= False

    send "i"
    matchf (limitRows [0] "Not carrying anything.") >>= \case
      True -> inventory .= []
      False -> do
        inv <- exhaustInventory
        inventory .= inv
 where
  exhaustInventory = do
    bottom_item <- matchf (regex "([0-9]+ of [0-9]+)|\\(end\\)")
    case bottom_item of
      Nothing -> return []
      Just dd -> do
        items <- fmap catMaybes $ for [0..y dd-1] $ \row -> do
          item_line <- T.strip <$> getScreenLine' row (x dd)
          return $
            if T.length item_line >= 5 &&
               T.index item_line 1 == ' ' &&
               T.index item_line 2 == '-' &&
               T.index item_line 3 == ' '
              then Just $ nameToItem (T.drop 4 item_line)
              else Nothing
        send " "
        (items <>) <$> exhaustInventory


inferCurrentLevel :: MonadAI m => [T.Text] -> StateT WorldState m ()
inferCurrentLevel messages = do
  inferLevelIndex

  inferInventory messages

  -- Does the level we are currently on "exist" (i.e. have we seen it before)?
  cl <- use currentLevel
  use (levels.at cl) >>= \case
    Just lvl -> do
      updated_lvl <- inferLevel lvl messages
      levels.at cl .= Just updated_lvl
    _ -> error "Impossible"

inferCurrentlyStandingSquare :: MonadAI m => Level -> [T.Text] -> StateT WorldState m Level
inferCurrentlyStandingSquare lvl msgs = do
  (_, cx, cy) <- currentScreen

  -- Check floor if
  -- 1) We don't know what level feature we are standing on.
  -- 2) There are items on the floor but we haven't checked which items
  -- exactly.
  -- 3) We get a message that suggests something is on the floor
  let lcell = lvl^?cells.ix (cx, cy)
  if (isNothing $ join $ lcell^?_Just.cellFeature) ||
     (lcell^?_Just.cellItems == Just PileSeen) ||
     (any (T.isInfixOf "You see here") msgs) ||
     (any (T.isInfixOf "You feel here") msgs)
    then checkFloor cx cy
    else return lvl
 where
  skipEngravings = do
    thing <- matchf (limitRows [0] $ regex "You read:|Something is written|Something is engraved")
    mm <- matchf "--More--"
    when (thing && mm) $ send " " >> skipEngravings

  checkFloor cx cy = do
    send ":"
    skipEngravings

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
        let i = nameToItem name
         in i `seq` return $ Pile [nameToItem name]
      _ -> do
        -- Multi item case then
        matchf (limitRows [0,2] $ regex "Things that you feel here:|Things that are here:") >>= \case
          Just dd -> itemInferLoop dd
          Nothing ->
            return NoPile

  itemInferLoop dd =
    matchf (limitRows [0] $ regex "Something is written|You read: |Something is engraved") >>= \case
      True -> bailout
      False -> itemInferLoop2 dd

  bailout = return $ NoPile

  itemInferLoop2 dd = do
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
nameToItem txt | T.isInfixOf "statue of" txt = Statue
nameToItem (T.strip -> txt'') = case name of
  "food ration" -> Food
  "food rations" -> Food
  _ -> StrangeItem
 where
  txt' = T.unpack txt''

  txt_article_removed = if isPrefixOf "a " txt'
                          then drop 2 txt'
                          else (if isPrefixOf "an " txt'
                                  then drop 3 txt'
                                  else (if isPrefixOf "the " txt'
                                          then drop 4 txt'
                                          else txt'))

  (quentity, txt_quantity_removed) = case txt_article_removed =~ ("([0-9]+) (.+)" :: String) of
    [[_whole, quantity, rest]] -> (read quantity, rest)

    _ -> (1, txt_article_removed)

  (buc, txt_buc_removed) = if isPrefixOf "uncursed " txt_article_removed
                             then (Just Uncursed, drop 9 txt_article_removed)
                             else (if isPrefixOf "blessed " txt_article_removed
                                     then (Just Blessed, drop 8 txt_article_removed)
                                     else (if isPrefixOf "cursed " txt_article_removed
                                             then (Just Cursed, drop 7 txt_article_removed)
                                             else (Nothing, txt_article_removed)))

  (enchantment, enchantment_removed) = case txt_buc_removed =~ ("(\\+|\\-)([0-9]+) (.+)" :: String) of
    [[_whole, sign, enchantment, rest]] ->
      (if sign == "-"
         then negate $ read enchantment
         else read enchantment
      ,rest)

    _ -> (0, txt_buc_removed)

  -- wizard mode
  weight_removed = case enchantment_removed =~ ("(.+) \\([0-9]+ aum\\)" :: String) of
    [[_whole, first_part]] -> first_part
    _ -> enchantment_removed

  name = weight_removed

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
isItemSymbol "[" _ = True
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

inferLevel :: MonadAI m => Level -> [T.Text] -> StateT WorldState m Level
inferLevel lvl msgs = do
  (ss, cx, cy) <- currentScreen
  statuses <- use statuses
  let (sw, sh) = screenSize ss

      new_cells = A.runSTArray $ do
                    mutcells <- A.thaw (lvl^.cells)
                    inferring cx cy sw sh statuses mutcells ss
                    return mutcells

      new_boulders = inferBoulders ss

      new_monsters = inferMonsters ss lvl (cx, cy) (lvl^.monsters)

  let updated_lvl = lvl & (cells .~ new_cells) .
                          (boulders .~ new_boulders) .
                          (monsters .~ new_monsters)

  inferCurrentlyStandingSquare updated_lvl msgs
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
      old_cell <- A.readArray mutcells (column, row)
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
            "7" | fcolor == LightGray -> Just Floor
            "7" | fcolor == Yellow && old_cell^.cellFeature /= Just LockedDoor
                  -> Just ClosedDoor
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
            " " | old_cell^.cellFeature /= Just Wall -> Just InkyBlackness
            _ -> Nothing

      case inferred of
        Nothing ->
          -- If we can't infer any new information then maybe we can infer
          -- something about items?
          when (isItemSymbol (contents cell) (foregroundColor cell)) $ do
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

levelSquares :: [(Int, Int)]
levelSquares = [ (x, y) | x <- [0..79], y <- [1..21] ]

-- Given screenstate and old state of monsters, update it and return new
-- state of monsters.
inferMonsters :: ScreenState -> Level -> (Int, Int) -> M.Map (Int, Int) MonsterImage -> M.Map (Int, Int) MonsterImage
inferMonsters ss lvl (cx, cy) monsters =
  -- Collect all squares that look like monsters
  let monster_squares = filter (\(x, y) -> looksLikeMonster lvl (x, y) $ getCell x y ss) levelSquares
      (unmoved_monsters, visited_old_monsters) = foldl' foldUnmovedMonsters (M.empty, S.empty) monster_squares
      (final_monsters, _) = foldl' foldMonsters (unmoved_monsters, visited_old_monsters) monster_squares
   in final_monsters
 where
  foldUnmovedMonsters (new_monsters, old_monsters_visited) (mx, my)
    | mx == cx && my == cy = (new_monsters, old_monsters_visited)
    | otherwise =

    let cell@Cell{..} = getCell mx my ss
     in case M.lookup (mx, my) monsters of
          Just mon | mon^.monsterAppearance == show cell ->
            (M.insert (mx, my) mon new_monsters, S.insert (mx, my) old_monsters_visited)
          _ -> (new_monsters, old_monsters_visited)

  foldMonsters (new_monsters, old_monsters_visited) (mx, my) | mx == cx && my == cy = (new_monsters, old_monsters_visited)
  foldMonsters (new_monsters, old_monsters_visited) (mx, my) | M.member (mx, my) new_monsters = (new_monsters, old_monsters_visited)
  foldMonsters (new_monsters, old_monsters_visited) (mx, my) =
    let cell@Cell{..} = getCell mx my ss
        newmon m = MonsterImage { _monster = m
                                , _isPeaceful = False
                                , _monsterAppearance = show cell }
        retmon (mi, new_mon) = (M.insert (mx, my) mi new_monsters, new_mon)

        -- If identical looking monster was next to this one (or at the same
        -- place) and no other monster is next to it, assume this is the same
        -- monster
        monsterMovedMaybe def =
          let candidates = M.fromList $ concat $ flip fmap [(x, y) | x <- [mx-1..mx+1], y <- [my-1..my+1], S.notMember (x, y) old_monsters_visited] $ \(x, y) ->
                             case M.lookup (x, y) monsters of
                               Nothing -> []
                               Just mi ->
                                 if mi^.monsterAppearance == show cell
                                   then [((x, y), mi)]
                                   else []
          in if M.size candidates == 1 -- We want unambiguous candidate so only copy the monster if there is one single candidate
               then (head $ M.elems candidates, S.insert (head $ M.keys candidates) old_monsters_visited)
               else (def, old_monsters_visited)

     in if | foregroundColor == Blue && contents == "e"
             -> retmon $ monsterMovedMaybe $ newmon FloatingEyeMonster
           | foregroundColor == Magenta && contents == "h"
             -> retmon $ monsterMovedMaybe $ newmon AmbiguousMonster
           | otherwise -> retmon $ monsterMovedMaybe $ newmon UnremarkableMonster

isMonsterSymbol :: Char -> Bool
isMonsterSymbol c = case c of
  ch | ch >= 'a' && ch <= 'z' -> True
  ch | ch >= 'A' && ch <= 'Z' -> True
  '&' -> True
  '\'' -> True
  ':' -> True
  ';' -> True
  '@' -> True
  '~' -> True
  ']' -> True
  _ -> False

looksLikeMonster :: Level -> (Int, Int) -> Cell -> Bool
looksLikeMonster _ _ (Cell{..}) | T.length contents /= 1 = False
looksLikeMonster _ _ (Cell{..}) | not (isMonsterSymbol $ T.head contents) = False
looksLikeMonster lvl pos (Cell{..}) =
  if | foregroundColor == White && backgroundColor == Black &&
       hasStatue lvl pos == True -> False
     | otherwise -> True

inferSquare :: MonadState WorldState m => (Int, Int) -> (LevelFeature -> LevelFeature) -> m ()
inferSquare pos modifier = do
  cl <- get
  put $ cl & levels.at (cl^.currentLevel)._Just.cells.ix pos.cellFeature._Just %~ modifier

inferPeaceful :: MonadState WorldState m => (Int, Int) -> m ()
inferPeaceful pos = do
  cl <- get
  put $ cl & levels.at (cl^.currentLevel)._Just.monsters.at pos._Just.isPeaceful .~ True

dirtyInventory :: MonadState WorldState m => m ()
dirtyInventory = inventoryDirty .= True

inferSearch :: (MonadState WorldState m) => (Int, Int) -> m ()
inferSearch (cx, cy) = do
  cl <- get
  for_ [(nx, ny) | nx <- [cx-1..cx+1], ny <- [cy-1..cy+1]] $ \npos ->
    modify $ levels.at (cl^.currentLevel)._Just.cells.ix npos.numberOfTimesSearched +~ 1

setLastSearchedPosition :: (MonadState WorldState m) => (Int, Int) -> m ()
setLastSearchedPosition (cx, cy) = do
  cl <- get
  modify $ levels.at (cl^.currentLevel)._Just.whereSearchedLastTime %~ \case
    Just (px, py, count) | px == cx && py == cy -> Just (px, py, count+1)
    _ -> Just (cx, cy, 0)

