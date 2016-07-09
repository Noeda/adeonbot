{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}

module Bot.NetHack.DecisionMaker
  ( decisionMaker )
  where

import Bot.NetHack.BFS
import Bot.NetHack.MonadAI
import Bot.NetHack.WorldState
import Bot.NetHack.InferWorldState ( levelSquares, inferSquare )
import Control.Lens hiding ( Level, levels )
import Control.Monad
import Control.Monad.State.Strict
import qualified Data.ByteString as B
import Data.Maybe
import qualified Data.Map.Strict as M
import Data.Monoid
import qualified Data.Set as S
import Terminal.Screen

decisionMaker :: Monad m => WAI m ()
decisionMaker = forever $ do
  let pursue f = do path <- f
                    (_, cx, cy) <- currentScreen
                    case path of
                      (p:_) -> case diffToDir (cx, cy) p of
                        Nothing -> empty
                        Just d -> send d
                      _ -> empty

  runAbortAI_ (pursue findMonsterKill <|>
               pursue findExplorablePath <|>
               findClosedDoors <|>
               findLockedDoors <|>
               error "nothing to do")

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

findClosedDoors :: (Alternative m, MonadWAI m)
                => m ()
findClosedDoors = do
  findWayToFeatures ClosedDoor
    (\d pos -> do
      send $ "o" <> d
      msgs <- askMessages
      when ("This door is locked." `elem` msgs) $
        modWorld $ execState $ inferSquare pos (\old_feature -> if old_feature == ClosedDoor
                                                  then LockedDoor
                                                  else old_feature))
    (\d _ -> send d)

findLockedDoors :: (Alternative m, MonadWAI m) => m ()
findLockedDoors =
  findWayToFeatures LockedDoor
    (\d _ -> send $ "\x04" <> d)
    (\d _ -> send d)

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

  let mset = M.keysSet (lvl^.monsters)
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

  -- Do a breadth-first search until at least one desirable is reached.
  al' $ levelSearch (cx, cy)
                    (\goal _ -> S.member goal desirables)
                    lvl

levelSearch :: (Int, Int)
            -> ((Int, Int) -> Maybe LevelCell -> Bool)
            -> Level
            -> Maybe [(Int, Int)]
levelSearch start_pos is_goal level =
  breadthFirstSearch start_pos
                     get_neighbours
                     (\(gx, gy) -> is_goal (gx, gy) (level^?cells.ix (gx, gy)))
 where
  get_neighbours (x, y) =
    let lst1 = [ (nx, ny) | nx <- [x-1..x+1], ny <- [y-1..y+1],
                  (nx /= x || ny /= y) &&
                   (fmap isPassable (join $ level^?cells.ix (nx, ny).cellFeature) == Just True ||
                    is_goal (nx, ny) Nothing) ]

        -- Filter diagonals when entering an open door
        -- WHY CAN'T YOU MOVE DIAGONALLY TO OPEN DOOR
     in filter diagonalFilter lst1
   where
    diagonalFilter (nx, ny)
      | nx /= x && ny /= y &&
        ( (join (level^?cells.ix (nx, ny).cellFeature) == Just OpenedDoor) ||
          (join (level^?cells.ix (x, y).cellFeature) == Just OpenedDoor) ) = False
    diagonalFilter _ = True


isDesirableExplorationTarget :: Level -> Int -> Int -> Bool
isDesirableExplorationTarget lvl x y =

  -- Case 1. places that are next to a black square that are not black squares
  -- themselves. (to explore reachable black squares).
  --
  -- The black square should be clean otherwise or we can't see the level
  -- feature even if we go there.
  ( isPassableSquare x y && (any (\(nx, ny) -> isExplorableBlackSquare nx ny) (neighboursOf x y))

    ||
  -- Case 2. Item piles that we haven't explored

    (lvl^?cells.ix (x, y).cellItems == Just PileSeen) )
 where
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

