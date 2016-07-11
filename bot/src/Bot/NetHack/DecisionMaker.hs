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
    runAbortAI_ (eatIfHungry <|>
                 pursue "Going towards monster at " findMonsterKill <|>
                 pursue "Going towards an explorable at " findExplorablePath <|>
                 findClosedDoors <|>
                 findLockedDoors <|>
                 findDownstairs <|>
                 searchAround <|>
                 error "nothing to do")

searchAround :: (Alternative m, MonadWAI m) => m ()
searchAround = do
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

    let reachable_squares = S.toList $ expand (cx, cy) (\pos -> getWalkableNeighbours lvl pos (const False))
  
    -- Which of those reachable squares is the best?
    --
    -- So we score each of them and then walk to the best square
    --
    -- Scoring currently works so that square with highest score is explored.
    --
    -- There are two heuristics, one is that we divide the level into 6 cells:
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
        case path of
          (p:_) -> case diffToDir (cx, cy) p of
            Nothing -> empty
            Just d -> send d
          _ -> empty

  searchableFolder :: M.Map (Int, Int) Int -> Level -> (Int, Int) -> Maybe ((Int, Int), Int) -> (Int, Int) -> Maybe ((Int, Int), Int)
  searchableFolder cell_scores lvl playerpos best_so_far pos@(px, py) =
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
    modify $ M.insert pos (negate search_penalty + bigcell_penalty)

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
  if Hungry `S.member` (wstate^.statuses)
    then tryEating
    else empty
 where
  tryEating = do
    sendRaw "e"
    line <- getScreenLine 0
    when (T.isInfixOf "; eat it?" line || T.isInfixOf "; eat one?" line) $
      logTrace "Saying no to eating corpse/food off ground" $ sendRaw "n"
    unless (T.isInfixOf "What do you want to eat" line) $
      logTrace ("Unexpected response to 'e' command: " <> show line) empty
    sendRaw "*"
    bsm <- selectItem (== Food)
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

al :: (Alternative m, MonadAI m) => m (Maybe a) -> m a
al action = do
  result <- action
  case result of
    Nothing -> empty
    Just ok -> return ok

al' :: (Alternative m, MonadAI m) => Maybe a -> m a
al' = \case
  Nothing -> empty
  Just ok -> return ok

findDownstairs :: (Alternative m, MonadWAI m)
               => m ()
findDownstairs = do
  wstate <- askWorldState
  lvl <- getCurrentLevel wstate
  (_, cx, cy) <- currentScreen

  if join (lvl^?cells.ix (cx, cy).cellFeature) == Just Downstairs
    then send ">"
    else findWayToFeatures Downstairs
           (\d _ -> send d)
           (\d _ -> send d)

findClosedDoors :: (Alternative m, MonadWAI m)
                => m ()
findClosedDoors = do
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
          modWorld $ execState $ inferSquare pos (\old_feature -> logTrace (show old_feature) $ if old_feature == ClosedDoor
                                                    then LockedDoor
                                                    else old_feature))
    (\d pos -> logTrace ("Moving towards closed door at. " <> show pos) $ send d)

findLockedDoors :: (Alternative m, MonadWAI m) => m ()
findLockedDoors =
  findWayToFeatures LockedDoor
    (\d pos -> logTrace ("Kicking door at " <> show (d, pos)) $ send $ "\x04" <> d)
    (\d pos -> logTrace ("Moving towards locked door at " <> show (d, pos)) $ send d)

findWayToFeatures :: (Alternative m, MonadWAI m)
                  => LevelFeature
                  -> (B.ByteString -> (Int, Int) -> m a)
                  -> (B.ByteString -> (Int, Int) -> m a)
                  -> m a
findWayToFeatures feature is_next towards_this_way = do
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

  let mset = M.keysSet $ M.filter (\m -> m^.isPeaceful == False &&
                                         m^.monster /= FloatingEyeMonster) $ (lvl^.monsters)
   in al' $
      levelSearch (cx, cy)
                  (\goal _ -> S.member goal mset && goal /= (cx, cy))
                  lvl

findExplorablePath :: (Alternative m, MonadWAI m) => m [(Int, Int)]
findExplorablePath = do
  wstate <- askWorldState
  (ss, cx, cy) <- currentScreen
  let (sw, sh) = screenSize ss
  lvl <- getCurrentLevel wstate

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

isWalkableTransition :: Level -> (Int, Int) -> (Int, Int) -> ((Int, Int) -> Bool) -> Bool
isWalkableTransition level (nx, ny) (x, y) exceptions =
  (fmap isPassable (join $ level^?cells.ix (nx, ny).cellFeature) == Just True &&
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
            -> Maybe [(Int, Int)]
levelSearch start_pos is_goal level =
  breadthFirstSearch start_pos
                     (\pos -> getWalkableNeighbours level pos (\npos -> is_goal npos Nothing))
                     (\(gx, gy) -> is_goal (gx, gy) (level^?cells.ix (gx, gy)))

getWalkableNeighbours :: Level -> (Int, Int) -> ((Int, Int) -> Bool) -> [(Int, Int)]
getWalkableNeighbours level (x, y) exceptions =
  [ (nx, ny) | nx <- [x-1..x+1], ny <- [y-1..y+1],
               (nx /= x || ny /= y) &&
                isWalkableTransition level (nx, ny) (x, y) exceptions ]
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
  isItemPassableSquare x y = fromMaybe False $ do
    cell <- lvl^?cells.ix (x, y)
    guard (S.notMember (x, y) $ lvl^.boulders)
    guard (not $ M.member (x, y) $ lvl^.monsters)
    case cell^.cellFeature of
      Just InkyBlackness -> return True
      Just feat -> return $ isPassable feat
      _ -> return True

  isPassableSquare x y = fromMaybe False $ do
    cell <- lvl^?cells.ix (x, y)
    guard (S.notMember (x, y) $ lvl^.boulders)
    guard (not $ M.member (x, y) $ lvl^.monsters)
    guard (fromMaybe False $ isPassable <$> (cell^.cellFeature))
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

