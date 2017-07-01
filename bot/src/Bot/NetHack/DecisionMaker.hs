{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Bot.NetHack.DecisionMaker
  ( decisionMaker )
  where

import Bot.NetHack.BFS
import Bot.NetHack.InferWorldState
import Bot.NetHack.Logs
import Bot.NetHack.MonadAI
import Bot.NetHack.SelectItem
import Bot.NetHack.WorldState
import Control.Lens hiding ( Level, levels )
import Control.Monad
import Control.Monad.State.Strict
import qualified Data.ByteString as B
import Data.Foldable
import qualified Data.IntMap.Strict as IM
import Data.Maybe
import qualified Data.Map.Strict as M
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as T
import Terminal.Screen

decisionMaker :: Monad m => WAI m ()
decisionMaker = forever $ do
  let pursue msg f = do path <- f
                        (_, cx, cy) <- currentScreen
                        case path of
                          (p:_) -> case diffToDir (cx, cy) p of
                            Nothing -> empty
                            Just d -> logTrace (msg <> show (last path)) $
                              lift $ withAnswerer "Really attack"
                                ((modWorld (execState $ inferPeaceful p)) >> send "n")
                                (send d)
                          _ -> empty

  withAnswerer "Call a"
    (send "\n") $
    runAbortAI_ (dywypi <|>
                 eatIfHungry <|>
                 pursue "Going towards monster at " findMonsterKill <|>
                 restIfLowHP <|>
                 pickUpSupplies <|>
                 pursue "Going towards an explorable at " findExplorablePath <|>
                 findClosedDoors <|>
                 findLockedDoors <|>
                 findDownstairs <|>
                 searchAround <|>
                 logError "nothing to do")

restIfLowHP :: (Alternative m, MonadWAI m) => m ()
restIfLowHP = do
  ws <- askWorldState
  -- Don't move if hp is less than 2/3 of maxHP
  if ws^.hp <= ((ws^.maxHP `div` 3) * 2)
    then send "s"
    else empty

dywypi :: (Alternative m, MonadWAI m) => m ()
dywypi = do
  line <- getScreenLine 0
  when (T.isInfixOf "Do you want your possessions identified?" line) $ do
    sendRaw "q"
    sendRaw " "
    sendRaw " "
    error "dywypi: I died"
  empty

count :: Foldable f => (a -> Bool) -> f a -> Int
count counter folding =
  foldl' (\val item -> if counter item then val+1 else val) 0 folding

pickUpSupplies :: (Alternative m, MonadWAI m) => m ()
pickUpSupplies = do
  -- Pick up food items if we have less than 5
  wstate <- askWorldState
  lvl <- getCurrentLevel wstate
  let inv = wstate^.inventory
      num_food_items = count isFood inv

  when (num_food_items >= 5) empty

  -- Find closest food item on the level
  let has_food iimage = case iimage of
                          Pile items -> any isFood items
                          _ -> False
      food_item_piles = S.fromList $
                        filter (\(x, y) -> fmap has_food (lvl^?cells.ix (x, y).cellItems) == Just True)
                               levelSquares

  -- If there are food piles then we can't pick up anything at this point :-(
  when (S.null food_item_piles) $
    logTrace "Would try to pick up food but no known food on the level." empty

  (_, cx, cy) <- currentScreen

  is_passable <- getPassableFunction

  path <- al' $ levelSearch (cx, cy)
                            (\goal _ -> S.member goal food_item_piles)
                            lvl
                            is_passable

  case path of
    [] -> do
      sendRaw ","
      line <- getScreenLine 0
      if | T.isInfixOf "There is nothing here to pick up." line ||
           T.isInfixOf "You could drink the water..." line
           -> modWorld (execState (inferItemPileImage (cx, cy) NoPile)) >> empty
         | otherwise
           -> do void $ selectManyItems (\item counter -> case item of
                   item | isFood item && counter < 5 ->
                     logTrace ("Picking up food item (" <> show counter <> " -> " <> show (counter+1) <> ")")
                              (1, counter+1)
                   _ -> (0, counter)) num_food_items
                 modWorld $ execState dirtyInventory
                 yield
    (p:_rest) -> do
      d <- al' $ diffToDir (cx, cy) p
      send d

getPassableFunction :: (Alternative m, MonadWAI m) => m (LevelFeature -> Bool)
getPassableFunction = do
  wstate <- askWorldState
  lvl <- getCurrentLevel wstate
  return $ if lvl^.numTurnsInSearchStrategy >= 500
             then isPassableIncludeTraps
             else isPassable

searchAround :: (Alternative m, MonadWAI m) => m ()
searchAround = do
  wstate <- askWorldState
  let curlvl = wstate^.currentLevel
  modWorld $ (levels.at curlvl._Just.numTurnsInSearchStrategy) +~ 1

  -- Expand from players position outwards to find all reachable walls
  (_, cx, cy) <- currentScreen

  wstate <- askWorldState
  lvl <- getCurrentLevel wstate

  -- If we searched last time already, search until we searched there at least
  -- 20 times
  case lvl^.whereSearchedLastTime of
    Just (px, py, count) | count < 20 && px == cx && py == cy ->
      logTrace ("Searching again in the same place (count " <> show count <> ")") $ do
        modWorld $ execState $ do
          inferSearch (cx, cy)
          setLastSearchedPosition (cx, cy)
        send "s"
    _ -> findNewSearchPosition
 where
  findNewSearchPosition = do
    (_, cx, cy) <- currentScreen
    wstate <- askWorldState
    lvl <- getCurrentLevel wstate

    is_passable <- getPassableFunction

    let reachable_squares = S.toList $ expand (cx, cy) (\pos -> getWalkableNeighbours lvl pos (const False) is_passable)
  
    -- Which of those reachable squares is the best?
    --
    -- So we score each of them and then walk to the best square
    --
    -- Scoring currently works so that square with highest score is explored.
    --
    -- There are some heuristics, one is that we divide the level into 6 cells:
    -- (see coords_to_cell/bigCellScores)
    --
    --   +--------+--------+--------+
    --   |        |        |        |
    --   |        |        |        |
    --   +--------+--------+--------|
    --   |        |        |        |
    --   |        |        |        |
    --   +--------+--------+--------+
    --
    --   Each cell gets a score; if a cell has lots of explored squares already
    --   (or searched a lot) then its score is lowered. The idea is that we'll
    --   explore parts of the level that look suspiciously empty.
    --
    --   The second heuristic lowers the score of any wall by the number of time
    --   it has been searched already.
    --
    --   Third heuristic:
    --
    --   If we see a corridor, with 5 sides as rock and one corridor behind it,
    --   we make that corridor much more desirable search target.
    --
    --   .|
    --   .+####
    --   .|   ^
    --      this spot here
    --
  
    let bigcell_scores = fmap (*10) $ bigCellScores lvl
        cell_scores = cellScores lvl bigcell_scores
  
    -- cell_scores now contains scores for every square on the level.
    -- Next, of all the explorable squares, find out the one that has highest
    -- cumulative score around it.
  
    case foldl' (searchableFolder cell_scores lvl (cx, cy)) Nothing (reachable_squares :: [(Int, Int)]) of
      Nothing -> empty
      Just (ppos, sc) | ppos == (cx, cy) -> do
        modWorld $ execState $ do
          inferSearch (cx, cy)
          setLastSearchedPosition (cx, cy)
        logTrace ("Searching at " <> show ppos <> ", has score " <> show sc) $ send "s"
      Just (ppos, _) -> do
        path <- al' $ levelSearch (cx, cy)
                                  (\goal _ -> goal == ppos)
                                  lvl
                                  is_passable

        case path of
          (p:_) -> case diffToDir (cx, cy) p of
            Nothing -> empty
            Just d -> send d
          _ -> empty

  searchableFolder :: M.Map (Int, Int) Int -> Level -> (Int, Int) -> Maybe ((Int, Int), Int) -> (Int, Int) -> Maybe ((Int, Int), Int)
  searchableFolder cell_scores lvl _playerpos best_so_far pos@(px, py) =
    let cumulative_score = sum $ flip fmap [(x, y) | x <- [px-1..px+1], y <- [py-1..py+1]] $ \(x, y) ->
                             let base_score = fromMaybe 0 (M.lookup (x, y) cell_scores)
                              in if join (lvl^?cells.ix (x, y).cellFeature) == Just Wall
                                   then base_score
                                   else base_score*5
     in case best_so_far of
          Nothing -> Just (pos, cumulative_score)
          Just (_, old_score) | old_score < cumulative_score -> Just (pos, cumulative_score)
          _ -> best_so_far

  cellScores lvl bigcell_scores = flip execState M.empty $ for_ levelSquares $ \pos -> do
    let search_penalty = (*200) $ fromMaybe 0 $ lvl^?cells.ix pos.numberOfTimesSearched
        bigcell_penalty = fromMaybe 0 $ IM.lookup (coords_to_cell pos) bigcell_scores
        corridor_boost = computeCorridorBoost lvl pos

    modify $ M.insert pos (negate search_penalty + bigcell_penalty + corridor_boost)

  computeCorridorBoost lvl pos@(x, y) = fromMaybe 0 $ do
    Floor <- lvl^?cells.ix pos.cellFeature._Just
    cell8 <- lvl^?cells.ix (x, y-1).cellFeature._Just
    cell4 <- lvl^?cells.ix (x-1, y).cellFeature._Just
    cell6 <- lvl^?cells.ix (x+1, y).cellFeature._Just
    cell2 <- lvl^?cells.ix (x, y+1).cellFeature._Just

    when (isPassable cell8 && isPassable cell2) empty
    when (isPassable cell4 && isPassable cell6) empty

    when (isPassable cell4 && isPassable cell8) empty
    when (isPassable cell6 && isPassable cell8) empty
    when (isPassable cell4 && isPassable cell2) empty
    when (isPassable cell6 && isPassable cell2) empty

    return 10

  bigCellScores lvl = flip execState IM.empty $ for_ levelSquares $ \pos -> do
    let cell_index = coords_to_cell pos

    when (join (lvl^?cells.ix pos.cellFeature) /= Nothing) $
      decreaseScore cell_index 1

    let search_penalty = fromMaybe 0 $ lvl^?cells.ix pos.numberOfTimesSearched
    decreaseScore cell_index search_penalty

  decreaseScore cell_index (negate -> penalty) = modify $ \old ->
    case IM.lookup cell_index old of
      Nothing -> IM.insert cell_index penalty old
      Just old_penalty -> IM.insert cell_index (penalty+old_penalty) old

  coords_to_cell (x, y)
   | x < 79 `div` 3       = if y <= 11 then 0 else 1
   | x < (79 `div` 3) * 2 = if y <= 11 then 2 else 3
   | otherwise            = if y <= 11 then 4 else 5

eatIfHungry :: (Alternative m, MonadWAI m) => m ()
eatIfHungry = do
  wstate <- askWorldState
  if Hungry `S.member` (wstate^.statuses) && hasFoodInInventory wstate
    then tryEating
    else empty
 where
  tryEating = do
    sendRaw "e"
    line <- getScreenLine 0
    when (T.isInfixOf "; eat it?" line || T.isInfixOf "; eat one?" line) $
      logTrace "Saying no to eating corpse/food off ground" $ sendRaw "n"
    line <- getScreenLine 0
    unless (T.isInfixOf "What do you want to eat" line) $
      logTrace ("Unexpected response to 'e' command: " <> show line) empty
    sendRaw "*"
    bsm <- selectItem isFood
    wstate <- askWorldState
    let inv = wstate^.inventory
    case bsm of
      Nothing -> logTrace ("No food to eat for hunger (inv: " <> show inv <> ")") $ sendRaw "\x1b" >> empty
      Just bs -> do
        modWorld $ execState dirtyInventory
        send bs

getCurrentLevel :: (Alternative m, Monad m) => WorldState -> m Level
getCurrentLevel wstate =
  let curlvl = wstate^.currentLevel
      mlvl = wstate^.levels.at curlvl
   in case mlvl of
        Nothing -> empty
        Just lvl -> return lvl

diffToDir :: (Int, Int) -> (Int, Int) -> Maybe B.ByteString
diffToDir (x1, y1) (x2, y2) =
  if | x1 == x2 && y1 == y2-1 -> Just "j"
     | x1 == x2 && y1 == y2+1 -> Just "k"
     | x1 == x2-1 && y1 == y2 -> Just "l"
     | x1 == x2+1 && y1 == y2 -> Just "h"
     | x1 == x2-1 && y1 == y2-1 -> Just "n"
     | x1 == x2+1 && y1 == y2-1 -> Just "b"
     | x1 == x2-1 && y1 == y2+1 -> Just "u"
     | x1 == x2+1 && y1 == y2+1 -> Just "y"
     | otherwise -> Nothing

al' :: (Alternative m, MonadAI m) => Maybe a -> m a
al' = \case
  Nothing -> empty
  Just ok -> return ok

findDownstairs :: (Alternative m, MonadWAI m)
               => m ()
findDownstairs = do
  is_passable <- getPassableFunction
  findDownstairs' is_passable

findDownstairs' :: (Alternative m, MonadWAI m)
                => (LevelFeature -> Bool)
                -> m ()
findDownstairs' is_passable = do
  wstate <- askWorldState
  lvl <- getCurrentLevel wstate
  (_, cx, cy) <- currentScreen

  if join (lvl^?cells.ix (cx, cy).cellFeature) == Just Downstairs
    then send ">"
    else findWayToFeatures Downstairs
           (\d _ -> send d)
           (\d _ -> send d)
           is_passable

findClosedDoors :: (Alternative m, MonadWAI m)
                => m ()
findClosedDoors = do
  is_passable <- getPassableFunction
  findWayToFeatures ClosedDoor
    (\d pos -> logTrace ("Next to closed door; will try to open. " <> show (d, pos)) $ do
      send $ "o" <> d
      msgs <- askMessages
      when ("This door is already open." `elem` msgs) $ do
        wstate <- askWorldState
        when (Confstunned `S.notMember` (wstate^.statuses)) $
          modWorld $ execState $ inferSquare pos (\_ -> OpenedDoor)
      when ("This door is locked." `elem` msgs) $
        logTrace ("Door is locked message received; inferring that door at " <> show pos <> " is locked.") $
          modWorld $ execState $ inferSquare pos (\old_feature -> if old_feature == ClosedDoor
                                                    then LockedDoor
                                                    else old_feature))
    (\d pos -> logTrace ("Moving towards closed door at. " <> show pos) $ send d)
    is_passable

findLockedDoors :: (Alternative m, MonadWAI m) => m ()
findLockedDoors = do
  wstate <- askWorldState
  when (hasSoreLegs wstate) empty
  is_passable <- getPassableFunction

  findWayToFeatures LockedDoor
    (\d pos -> do
        logTrace ("Kicking door at " <> show (d, pos)) $ send $ "\x04" <> d
        msgs <- askMessages
        when (any (T.isInfixOf "is in no shape for kicking") msgs) $ do
          modWorld $ execState $ inferSoreLegs 40
          empty)
    (\d pos -> logTrace ("Moving towards locked door at " <> show (d, pos)) $ send d)
    is_passable

findWayToFeatures :: (Alternative m, MonadWAI m)
                  => LevelFeature
                  -> (B.ByteString -> (Int, Int) -> m a)
                  -> (B.ByteString -> (Int, Int) -> m a)
                  -> (LevelFeature -> Bool)
                  -> m a
findWayToFeatures feature is_next towards_this_way is_passable = do
  wstate <- askWorldState
  lvl <- getCurrentLevel wstate
  (_, cx, cy) <- currentScreen

  let targets = S.delete (cx, cy) $
                S.fromList $
                filter (\(x, y) -> join (lvl^?cells.ix (x, y).cellFeature) == Just feature)
                       levelSquares

  path <- al' $ levelSearch (cx, cy)
                            (\goal _ -> S.member goal targets)
                            lvl
                            is_passable

  case path of
    [next_to_me] -> do
      d <- al' $ diffToDir (cx, cy) next_to_me
      is_next d next_to_me
    (p:_rest) -> do
      d <- al' $ diffToDir (cx, cy) p
      towards_this_way d p
    _ -> empty

findMonsterKill :: (Alternative m, MonadWAI m) => m [(Int, Int)]
findMonsterKill = do
  wstate <- askWorldState
  (_, cx, cy) <- currentScreen
  lvl <- getCurrentLevel wstate

  is_passable <- getPassableFunction

  let mset = M.keysSet $ M.filter (\m -> m^.isPeaceful == Just False &&
                                         m^.monster /= FloatingEyeMonster) $ (lvl^.monsters)
   in al' $
      levelSearch (cx, cy)
                  (\goal _ -> S.member goal mset && goal /= (cx, cy))
                  lvl
                  is_passable

findExplorablePath :: (Alternative m, MonadWAI m) => m [(Int, Int)]
findExplorablePath = do
  wstate <- askWorldState
  (ss, cx, cy) <- currentScreen
  let (sw, sh) = screenSize ss
  lvl <- getCurrentLevel wstate
  is_passable <- getPassableFunction

  -- Collect all places on the level that look "desirable" as exploration
  -- targets.
  let desirables = S.delete (cx, cy) $
                   S.fromList $
                   filter (\(x, y) -> isDesirableExplorationTarget lvl x y)
                          [ (x, y) | x <- [0..sw-1], y <- [1..sh-3] ]

  let unexplored_piles = S.size $ S.filter (\(x, y) -> lvl^?cells.ix (x, y).cellItems == (Just PileSeen)) (S.fromList [ (x, y) | x <- [0..sw-1], y <- [1..sh-3] ])

  -- Do a breadth-first search until at least one desirable is reached.
  logTrace ("Number of unseen item piles is " <> show unexplored_piles <> ", list of desirables: " <> show desirables) $
    al' $ levelSearch (cx, cy)
                      (\goal _ -> S.member goal desirables)
                      lvl
                      is_passable

isWalkableTransition :: Level -> (Int, Int) -> (Int, Int) -> ((Int, Int) -> Bool) -> (LevelFeature -> Bool) -> Bool
isWalkableTransition level (nx, ny) (x, y) exceptions is_passable =
  (fmap is_passable (join $ level^?cells.ix (nx, ny).cellFeature) == Just True &&
   (S.notMember (nx, ny) $ level^.boulders) &&
   join (level^?monsters.at (nx, ny)) == Nothing &&
   diagonalFilter (nx, ny) (x, y)) ||
   (exceptions (nx, ny) && diagonalFilter (nx, ny) (x, y))
 where
  diagonalFilter (nx, ny) (x, y) =
    (nx == x || ny == y) ||
    ((join (level^?cells.ix (nx, ny).cellFeature) /= Just OpenedDoor) &&
     (join (level^?cells.ix (x, y).cellFeature) /= Just OpenedDoor))
{-# INLINE isWalkableTransition #-}

levelSearch :: (Int, Int)
            -> ((Int, Int) -> Maybe LevelCell -> Bool)
            -> Level
            -> (LevelFeature -> Bool)
            -> Maybe [(Int, Int)]
levelSearch start_pos is_goal level is_passable =
  breadthFirstSearch start_pos
                     (\pos -> getWalkableNeighbours level pos (\npos -> is_goal npos Nothing) is_passable)
                     (\(gx, gy) -> is_goal (gx, gy) (level^?cells.ix (gx, gy)))

getWalkableNeighbours :: Level -> (Int, Int) -> ((Int, Int) -> Bool) -> (LevelFeature -> Bool) -> [(Int, Int)]
getWalkableNeighbours level (x, y) exceptions is_passable =
  [ (nx, ny) | nx <- [x-1..x+1], ny <- [y-1..y+1],
               (nx /= x || ny /= y) &&
                isWalkableTransition level (nx, ny) (x, y) exceptions is_passable ]
{-# INLINE getWalkableNeighbours #-}

isDesirableExplorationTarget :: Level -> Int -> Int -> Bool
isDesirableExplorationTarget lvl x y =

  -- Case 1. places that are next to a black square that are not black squares
  -- themselves. (to explore reachable black squares).
  --
  -- The black square should be clean otherwise or we can't see the level
  -- feature even if we go there.
  (isPassableSquare x y && (any (\(nx, ny) -> isExplorableBlackSquare nx ny) (neighboursOf x y)))

    ||
  -- Case 2. Item piles that we haven't explored

    (isItemPassableSquare x y && lvl^?cells.ix (x, y).cellItems == Just PileSeen)
 where
  is_passable = if lvl^.numTurnsInSearchStrategy >= 500
                  then isPassableIncludeTraps
                  else isPassable

  isItemPassableSquare x y = fromMaybe False $ do
    cell <- lvl^?cells.ix (x, y)
    guard (S.notMember (x, y) $ lvl^.boulders)
    guard (not $ M.member (x, y) $ lvl^.monsters)
    case cell^.cellFeature of
      Just InkyBlackness -> return True
      Just feat -> return $ is_passable feat
      _ -> return True

  isPassableSquare x y = fromMaybe False $ do
    cell <- lvl^?cells.ix (x, y)
    guard (S.notMember (x, y) $ lvl^.boulders)
    guard (not $ M.member (x, y) $ lvl^.monsters)
    guard (fromMaybe False $ is_passable <$> (cell^.cellFeature))
    return True

  isExplorableBlackSquare x y = fromMaybe False $ do
    cell <- lvl^?cells.ix (x, y)
    guard (S.notMember (x, y) $ lvl^.boulders)
    guard (not $ M.member (x, y) $ lvl^.monsters)
    guard (cell^.cellItems == NoPile)
    guard (cell^.cellFeature == Just InkyBlackness)
    return True

neighboursOf :: Int -> Int -> [(Int, Int)]
neighboursOf x y =
  [(nx, ny) | nx <- [x-1..x+1], ny <- [y-1..y+1], nx /= x || ny /= y]

