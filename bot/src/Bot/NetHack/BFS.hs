{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Bot.NetHack.BFS
  ( breadthFirstSearch
  , astarSearch
  , expand )
  where

import Data.Foldable
import Data.Graph.AStar
import Data.Hashable
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- | Similar to `breadthFirstSearch` but uses A* instead.
--
-- You need to supply end goal target point and a heuristic function for this
-- to work so it's not a drop-in replacement for `breadthFirstSearch`.
astarSearch :: (Hashable a, Ord a, Ord c, Num c)
            => a
            -> (a -> [a])
            -> a              -- ^ Goal
            -> (a -> c)       -- ^ Heuristic, distance to goal
            -> Maybe [a]
astarSearch start neighbours goal heuristic =
  aStar (\v -> HS.fromList $ neighbours v)
        (\_ _ -> 1)
        heuristic
        (== goal)
        start

-- | Breadth-first search. sUCH...GENERUC!!!!
breadthFirstSearch :: forall a. Ord a
                   => a           -- ^ Starting point.
                   -> (a -> [a])  -- ^ Get neighbours of a point.
                   -> (a -> Bool) -- ^ Is this a goal point?
                   -> Maybe [a]   -- ^ Path from starting point to goal point.
breadthFirstSearch start neighbours is_goal =
  fmap reverse $ iteration M.empty (M.singleton start 0) M.empty 1
 where
  iteration :: M.Map a Int -> M.Map a Int -> M.Map a a -> Int -> Maybe [a]
  iteration visited_points visit_points reverse_map distance_index =
    case M.filterWithKey (\pos _ -> is_goal pos) visit_points of
      map | Just ((pos, _), _) <- M.minViewWithKey map ->
        Just $ backTrack reverse_map pos
      _ ->
        let new_visited_points = M.union visit_points visited_points
            (visit_these_points_next, new_reverse_map) =
              foldl' (visiterFolder distance_index new_visited_points) (M.empty, reverse_map) (M.keys visit_points)
         in if M.null visit_these_points_next
              then Nothing
              else iteration new_visited_points visit_these_points_next new_reverse_map (distance_index+1)

  visiterFolder distance_index new_visited_points (visit_these_points_next, reverse_map) visit_point =
    let n = neighbours visit_point
     in foldl' folder (visit_these_points_next, reverse_map) n
   where
    folder (visit_these_points_next, reverse_map) neighbour =
      case M.lookup neighbour new_visited_points of
        Nothing ->
          let m1 = M.insert neighbour distance_index visit_these_points_next
              m2 = M.insert neighbour visit_point reverse_map
           in m1 `seq` m2 `seq` (m1, m2)
        Just{} -> (visit_these_points_next, reverse_map)

  backTrack :: M.Map a a -> a -> [a]
  backTrack reverse_map pos = case M.lookup pos reverse_map of
    Nothing -> []
    Just next_pos -> pos:backTrack reverse_map next_pos

-- | Expands to all found neighbours.
expand :: forall a. Ord a
       => a           -- ^ Staring point.
       -> (a -> [a])  -- ^ Get neighbours of a point.
       -> S.Set a     -- ^ All reachable points, including starting point.
expand start_point neighbours = iteration (S.singleton start_point) (S.singleton start_point)
 where
  iteration visiting_points !visited_points | S.null visiting_points = visited_points
  iteration visiting_points !visited_points =
    let visit_points_next_time = S.difference (foldl' (\set i -> S.insert i set) S.empty (concat $ fmap neighbours $ S.toList visiting_points))
                                              visited_points
        visited_points_next_time = S.union visit_points_next_time visited_points
     in iteration visit_points_next_time visited_points_next_time
