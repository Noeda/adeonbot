{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}

-- This module implements functions that try to figure out how to move and
-- attack in a fight.
--
-- Mostly it's about retreating into corridors or corners when faced by
-- multiple adversaries.
--
-- This module is written so that it doesn't depend on other modules in
-- NetHack. This makes it easier to test it offline independently.

module Bot.NetHack.FightingTactics
  ( FightingConditions()
  , Obstacle(..)
  , mkFightingConditions
  , computeBestMoveLocation

  , debugSimulation )
  where

import Bot.NetHack.BFS
import Control.Lens
import Control.Monad.State.Strict
import Control.Parallel.Strategies
import Data.Data
import Data.Foldable
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ord
import qualified Data.Set as S
import GHC.Generics
import System.IO

data Obstacle = Obstacle | Floor | CardinalFloor
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

-- This data tracks information about a monster
data Monster = Monster
  { _monsterHP :: !Double }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

-- This data holds the fighting conditions (how much hp, where monsters are,
-- what the world looks lik etc.)
data FightingConditions = FightingConditions
  -- Count how much damage we have received so far.
  { _damageReceived :: !Int

  -- Count how much damage we have inflicted so far.
  , _damageInflicted :: !Int

  -- Where is the player?
  , _playerLocation :: !(Int, Int)

  -- Turn count.
  , _turnCount :: !Double

  -- Maximum turn count. This puts a limit how "deep" we search in our graph
  , _maxTurnCount :: !Double

  -- Mapping of obstascles. No key in the map = obstacle is floor. This way,
  -- the world is an infinite plane of Walls.
  --
  -- It needs to be a wall or our searching algorithm becomes challenging to
  -- constraint not to try find paths through infinite plane.
  , _worldRaw :: !(M.Map (Int, Int) Obstacle)

  -- Mapping of monsters.
  , _monstersRaw :: !(M.Map (Int, Int) Monster) }
makeLenses ''FightingConditions
makeLenses ''Monster

emptyFightingConditions :: FightingConditions
emptyFightingConditions = FightingConditions
  { _damageReceived  = 0
  , _damageInflicted = 0
  , _playerLocation  = (0, 0)
  , _worldRaw        = M.empty
  , _monstersRaw     = M.empty
  , _turnCount       = 0
  , _maxTurnCount    = 10 }

mkFightingConditions :: (Int, Int)
                     -> M.Map (Int, Int) Obstacle
                     -> S.Set (Int, Int)
                     -> FightingConditions
mkFightingConditions player_location obstacles monsters =
  emptyFightingConditions &
    (playerLocation .~ player_location) .
    (worldRaw .~ obstacles) .
    (monstersRaw .~ (M.fromList $ S.toList monsters <&> \monpos -> (monpos, Monster 4)))

obstacleAt :: (Int, Int) -> Lens' FightingConditions Obstacle
obstacleAt (x, y) = lens get_it set_it
 where
  get_it fc = fromMaybe Obstacle $ fc^.worldRaw.at (x, y)
  set_it fc Obstacle = fc & worldRaw.at (x, y) .~ Nothing
  set_it fc feat = fc & worldRaw.at (x, y) .~ Just feat
{-# INLINE obstacleAt #-}

neighbours :: FightingConditions -> ((Int, Int) -> [(Int, Int)])
neighbours fc pos@(x, y) =
  filter filterByCardinal $
  case fc^.obstacleAt pos of
    CardinalFloor ->
      [(x+1, y), (x-1, y), (x, y-1), (x, y+1)]
    _ ->
      [(x+1, y), (x-1, y), (x, y-1), (x, y+1)
      ,(x+1, y+1), (x+1, y-1), (x-1, y+1), (x-1, y+1)]
 where
  filterByCardinal tpos@(tx, ty) =
    (case fc^.obstacleAt pos of
       CardinalFloor -> tx == x || ty == y
       _ -> True) &&
    case fc^.obstacleAt tpos of
      Floor -> True
      CardinalFloor | tx == x || ty == y -> True
      _ -> False
{-# INLINE neighbours #-}

haveMonstersHitPlayers :: FightingConditions -> FightingConditions
haveMonstersHitPlayers fc = flip execState fc $
  for_ [(x, y) | x <- [-1..1], y <- [-1..1], not (x == 0 && y == 0)] $ \(x', y') -> do
    let x = x' + (fc^.playerLocation._1)
        y = y' + (fc^.playerLocation._2)

    when (isJust $ fc^.monstersRaw.at (x, y)) $
      damageReceived += 1

havePlayerHitMonsters :: FightingConditions -> FightingConditions
havePlayerHitMonsters fc = flip execState fc $ do
  let monster_coords =
        concat $
        [(x, y) | x <- [-1..1], y <- [-1..1], not (x == 0 && y == 0)] <&> \(x', y') ->
          let x = x' + (fc^.playerLocation._1)
              y = y' + (fc^.playerLocation._2)

           in case fc^.monstersRaw.at (x, y) of
                Nothing -> []
                Just{}  -> [(x, y)]

  unless (null monster_coords) $ do
    damageInflicted += 1

    -- Divide 1 damage for all monsters adjacent to the player
    let damage_per_monster = 1/(fromIntegral $ length monster_coords)
    for_ monster_coords $ \mpos -> do
      monstersRaw.at mpos._Just.monsterHP -= damage_per_monster
      dam_now <- (^?monstersRaw.at mpos._Just.monsterHP) <$> get
      case dam_now of
        Just dn | dn <= 0 -> monstersRaw.at mpos .= Nothing
        _ -> return ()

moveMonsters :: FightingConditions
             -> FightingConditions
moveMonsters fc = flip execState fc $ do
  go (M.keysSet $ fc^.monstersRaw)
 where
  go monsters_still_left_to_move = do
    let path_ignoring_monsters = breadthFirstSearch (fc^.playerLocation)
                                 (neighbours fc)
                                 (\p -> p `S.member` monsters_still_left_to_move &&
                                        p `M.member` (fc^.monstersRaw))
        path_assuming_monsters = breadthFirstSearch (fc^.playerLocation)
                                 (\pos -> filter (\npos -> npos `S.member` monsters_still_left_to_move || npos `M.notMember` (fc^.monstersRaw)) $ neighbours fc pos)
                                 (\p -> p `S.member` monsters_still_left_to_move &&
                                        p `M.member` (fc^.monstersRaw))

    -- Find the closest monster to player, and move them first.
    case path_ignoring_monsters of
      -- No monsters are reachable. Monsters can't hurt us! HAHAHAHAHAHAA
      Just path | length path > 1 -> do
        let monster_pos = last path
            monster_pos_tgt = last $ init path
        -- If the target square is free of monsters, move there, otherwise
        -- block itself.
        use (monstersRaw.at monster_pos_tgt) >>= \case
          Nothing -> do
            old_mon <- use $ monstersRaw.at monster_pos
            monstersRaw.at monster_pos .= Nothing
            monstersRaw.at monster_pos_tgt .= old_mon

          -- Is a monster blocking our way? Then try the path that *doesn't*
          -- ignore the presence of monsters
          _ -> case path_assuming_monsters of
            Just path | length path > 1 -> do
              let monster_pos = last path
                  monster_pos_tgt = last $ init path

              use (monstersRaw.at monster_pos_tgt) >>= \case
                Nothing -> do
                  old_mon <- use $ monstersRaw.at monster_pos
                  monstersRaw.at monster_pos .= Nothing
                  monstersRaw.at monster_pos_tgt .= old_mon
                _ -> return ()

            Just path -> do
              let monster_pos = last path
              go (S.delete monster_pos monsters_still_left_to_move)

            _ -> return ()

        go (S.delete monster_pos monsters_still_left_to_move)

      Just path -> do
        let monster_pos = last path
        go (S.delete monster_pos monsters_still_left_to_move)

      -- If we get here, it means the remaining monsters are not reachable.
      -- Delete those monsters.
      _ -> for_ monsters_still_left_to_move $ \mpos ->
             monstersRaw.at mpos .= Nothing

tickSimulation' :: Bool -> FightingConditions -> FightingConditions
tickSimulation' player_can_hit_monsters =
  (if player_can_hit_monsters
     then havePlayerHitMonsters
     else id) .
  haveMonstersHitPlayers .
  moveMonsters

computeBestMoveLocation :: FightingConditions -> (Int, Int)
computeBestMoveLocation fc' =
  let candidate_targets = S.toList $ expand (fc^.playerLocation) (ne fc)
      scored_candidates' = (\tgt -> (tgt, scoreCandidate tgt)) <$> candidate_targets
      scored_candidates =
        withStrategy (parListChunk 5 rdeepseq) scored_candidates'
      best_candidate = fst $ minimumBy (comparing snd) scored_candidates

   in if best_candidate /= (fc^.playerLocation)
        then head $ fromJust $ astarSearch (fc^.playerLocation)
                                           (ne fc)
                                           best_candidate
                                           (distHeuristic best_candidate)
        else best_candidate
 where
  fc = fc' & (turnCount .~ 0) .
             (damageReceived .~ 0) .
             (damageInflicted .~ 0)

  ne fc pos = filter (\npos -> npos `M.notMember` (fc^.monstersRaw)) $ neighbours fc pos

  scoreCandidate tgt = go fc
   where
    go fc | M.null (fc^.monstersRaw) || fc^.turnCount >= fc^.maxTurnCount =
      (if tgt == fc^.playerLocation
         then (-5)
         else 0) +
      negate (fromIntegral (fc^.damageInflicted)) +
      fromIntegral (fc^.damageReceived) +
      ((fc^.turnCount) * 0.0001)
    go fc =
      let fc1 = if fc^.playerLocation /= tgt
                  then case astarSearch (fc^.playerLocation)
                                        (ne fc)
                                        tgt
                                        (distHeuristic tgt) of
                         Nothing -> tickSimulation' True fc
                         Just (p:_) -> tickSimulation' False $ fc & playerLocation .~ p
                         _ -> error "impossible"
                  else tickSimulation' True fc

       in go (fc1 & turnCount +~ 1)

  distHeuristic (x1, y1) (x2, y2) = min (abs (x2-x1)) (abs (y2-y1))

-- This is a function you can from ghci to play around.
-- It starts a scenario with bunch of monsters and prints out a crude ASCII art
-- depicting the simulation.
debugSimulation :: IO ()
debugSimulation = go initial_conditions
 where
  floorie x y =
    if | y == 3 && x > 2 -> Obstacle
       | x == -4 && y > -1 && y < 3 -> Obstacle
       | x < -3    -> Floor
       | y /= -3   -> Floor
       | otherwise -> Obstacle

  initial_conditions = emptyFightingConditions
    { _monstersRaw =
        M.fromList ([(x, -5) | x <- [-5..5]] <&> \pos -> (pos, Monster 10))
        `M.union`
        M.fromList ([((3, 5), Monster 10), ((4, 5), Monster 10)])
    , _worldRaw =
        M.fromList [((x, y), floorie x y) | x <- [-7..7], y <- [-7..7]]
     }

  go fc = do
    let mi_x = (minimum $ fmap fst $ M.keys (fc^.worldRaw)) - 1
        ma_x = (maximum $ fmap fst $ M.keys (fc^.worldRaw)) + 1
        mi_y = (minimum $ fmap snd $ M.keys (fc^.worldRaw)) - 1
        ma_y = (maximum $ fmap snd $ M.keys (fc^.worldRaw)) + 1

    for_ [mi_y..ma_y] $ \y -> do
      for_ [mi_x..ma_x] $ \x -> do
        let ch = if (x, y) == fc^.playerLocation
                   then '@'
                   else case fc^.monstersRaw.at (x, y) of
                          Nothing -> obstacleToChar $ fc^.obstacleAt (x, y)
                          Just _  -> 'M'
        putStr [ch]
      putStrLn ""
    putStrLn "-----"
    hFlush stdout

    let new_loc = computeBestMoveLocation fc
        new_fc =
          if new_loc == fc^.playerLocation
            then tickSimulation' True fc
            else tickSimulation' False $ fc & playerLocation .~ new_loc

    if new_fc^.monstersRaw /= fc^.monstersRaw
      then go new_fc
      else return ()

  obstacleToChar Floor = '.'
  obstacleToChar Obstacle = '#'
  obstacleToChar CardinalFloor = '+'
