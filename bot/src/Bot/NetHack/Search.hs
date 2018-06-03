{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.NetHack.Search
  ( levelSearch
  , worldSearch
  , getWalkableNeighbours
  , isWalkableTransition
  , WorldSearchResult(..) )
  where

import Bot.NetHack.BFS
import Bot.NetHack.Direction
import Bot.NetHack.WorldState
import Control.Lens hiding ( levels, Level )
import Control.Monad
import Data.Data
import qualified Data.Map.Strict as M
import Data.Monoid
import qualified Data.Set as S
import Data.Maybe
import GHC.Generics

data WorldSearchResult
  = AtTarget
  | MoveToTarget Direction [(Int, Int)]
  | ChangeLevel
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

levelSearch :: (Int, Int)
            -> ((Int, Int) -> Maybe LevelCell -> Bool)
            -> Level
            -> (LevelFeature -> Bool)
            -> Maybe [(Int, Int)]
levelSearch start_pos is_goal level is_passable =
  breadthFirstSearch start_pos
                     (\pos -> getWalkableNeighbours level pos (\npos -> is_goal npos (level^?cells.ix npos)) is_passable)
                     (\(gx, gy) -> is_goal (gx, gy) (level^?cells.ix (gx, gy)))

getWalkableNeighbours :: Level
                      -> (Int, Int)
                      -> ((Int, Int) -> Bool)
                      -> (LevelFeature -> Bool)
                      -> [(Int, Int)]
getWalkableNeighbours level (x, y) exceptions is_passable =
  [ (nx, ny) | nx <- [x-1..x+1], ny <- [y-1..y+1],
               (nx /= x || ny /= y) &&
                isWalkableTransition level (nx, ny) (x, y) exceptions is_passable ]
{-# INLINE getWalkableNeighbours #-}

data WorldPos = WorldPos LevelIndex (Int, Int)
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

worldPosCoords :: WorldPos -> (Int, Int)
worldPosCoords (WorldPos _ pos) = pos

worldPosNeighbours :: WorldState
                   -> (LevelCell -> Bool)
                   -> (LevelFeature -> Bool)
                   -> WorldPos
                   -> [WorldPos]
worldPosNeighbours wstate is_goal is_passable (WorldPos level_idx pos) =
  world_neighbours <> level_neighbours
 where
  level_neighbours =
    fmap (\coords -> WorldPos level_idx coords) $
    getWalkableNeighbours lvl pos exceptions is_passable

  world_neighbours :: [WorldPos]
  world_neighbours = fromMaybe [] $ do
    (tgt_lvl, (tx, ty)) <- getConnection pos lvl
    return [WorldPos tgt_lvl (tx, ty)]

  lvl = fromJust $ wstate^.levels.at level_idx

  exceptions = (\npos -> fromMaybe False $ is_goal <$> (lvl^?cells.ix npos))

-- Same as level search but searches by entire known dungeon
worldSearch :: (Int, Int)
            -> (LevelCell -> Bool)
            -> WorldState
            -> (LevelFeature -> Bool)
            -> Maybe WorldSearchResult
worldSearch start_pos is_goal wstate is_passable = do
  curlvl <- firstOf currentLevelT wstate
  let curlvl_idx = wstate^.currentLevel
  -- Does the current level have what we need? And can we reach it?
  case levelSearch start_pos (\_ cell -> case cell of Nothing -> False; Just c -> is_goal c) curlvl is_passable of
    Nothing ->
      case expensiveSearch of
        Nothing -> Nothing
        Just [] -> return AtTarget
        Just path ->
          let first_step = head path
           in case first_step of
                -- Do we need to do some walking on the same level we are now?
                WorldPos lvl_idx (tx, ty) | lvl_idx == curlvl_idx ->
                  return $ MoveToTarget (fromMaybe (error "worldSearch MoveToTarget across levels failed") $ diffToDir start_pos (tx, ty))
                                        (fmap worldPosCoords $ filter (\(WorldPos lvl_idx _) -> lvl_idx == curlvl_idx) path)

                -- This clause should trigger if we are about to use
                -- up/downstairs
                _ -> return ChangeLevel

    Just [] -> return AtTarget

    Just path ->
      return $ MoveToTarget (fromMaybe (error "worldSearch MoveToTarget build failed") $ diffToDir start_pos (head path))
                            path
 where
  expensiveSearch =
    breadthFirstSearch (WorldPos (wstate^.currentLevel) start_pos)
                       (worldPosNeighbours wstate is_goal is_passable)
                       (\(WorldPos level_index (cx, cy)) ->
                           let lvl = fromJust $ wstate^.levels.at level_index
                            in fromMaybe False $ is_goal <$> (firstOf (cells.ix (cx, cy)) lvl))

isWalkableTransition :: Level -> (Int, Int) -> (Int, Int) -> ((Int, Int) -> Bool) -> (LevelFeature -> Bool) -> Bool
isWalkableTransition level (nx, ny) (x, y) exceptions is_passable =
  not_failed_walk_count_excluded && rest

 where
  not_failed_walk_count_excluded = fromMaybe True $ do
    mmap <- M.lookup (x, y) (level^.failedWalks)
    dir <- diffToDir (x, y) (nx, ny)
    (_failed_turn, count) <- M.lookup dir mmap
    return $ not (count >= 10)

  rest =
    (fmap is_passable (join $ level^?cells.ix (nx, ny).cellFeature) == Just True &&
     (S.notMember (nx, ny) $ level^.boulders) &&
       join (level^?monsters.at (nx, ny)) == Nothing &&
       diagonalFilter (nx, ny) (x, y)) ||

     (exceptions (nx, ny) &&
      diagonalFilter (nx, ny) (x, y))

  diagonalFilter (nx, ny) (x, y) =
    (nx == x || ny == y) ||
    ((join (level^?cells.ix (nx, ny).cellFeature) /= Just OpenedDoor) &&
     (join (level^?cells.ix (x, y).cellFeature) /= Just OpenedDoor))
{-# INLINE isWalkableTransition #-}

