{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.NetHack.WorldState
  ( WorldState(..)
  , LevelIndex
  , emptyWorldState
  , levels
  , statuses
  , currentLevel
  , turn
  , Level(..)
  , emptyLevel
  , cells
  , boulders
  , monsters
  , LevelFeature(..)
  , LevelCell(..)
  , cellFeature
  , cellItems
  , Monster(..)
  , Status(..)
  , ItemPileImage(..)
  , ItemPile
  , Item(..) )
  where

import Control.Lens hiding ( Level, levels )
import qualified Data.Array.IArray as A
import Data.Data
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import GHC.Generics

type LevelIndex = Int

-- | This data represents what the bot thinks the world state is.
--
-- Kept in easily serializable form so bot's view of the world can be easily
-- saved.
data WorldState = WorldState
  { _levels       :: !(IM.IntMap Level)
  , _currentLevel :: !LevelIndex
  , _statuses     :: !(S.Set Status)
  , _turn         :: !Int }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

data Level = Level
  { _cells    :: !(A.Array (Int, Int) LevelCell)
  , _boulders :: !(S.Set (Int, Int))
  , _monsters :: !(M.Map (Int, Int) Monster) }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

data LevelCell = LevelCell
  { _cellFeature :: !(Maybe LevelFeature)
  , _cellItems :: !ItemPileImage }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

data LevelFeature
  = Wall                   -- Any wall like thing, can be black rock or | or -
  | Floor                  -- Floor things
  | Downstairs
  | Upstairs
  | Fountain               -- For all your quaffing needs
  | ClosedDoor
  | OpenedDoor
  | Altar
  | Trap
  | Lava
  | Water
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

data Monster
  = AmbiguousMonster       -- We see a monster here but don't know what type
                           -- it is exactly. Happens with purple 'h' monstly,
                           -- don't know if it's dwarf king of mind flayer.
  | UnremarkableMonster    -- Moves towards you and hits you. Most monsters are of this type.
  | FloatingEyeMonster     -- If you hit it directly, you get frozen.
  | MindFlayerMonster      -- Evil stuff.
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

data Status
  = Confstunned            -- Confused or stunned. Right now we treat them the same.
  | Blind                  -- Blind. Can't infer things about surroundings the same way.
  | FoodPoisoning          -- Oh my, what did you eat??? BAD BOT
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

data ItemPileImage
  = NoPile         -- We don't see any item pile here
  | PileSeen       -- We see a pile of items but don't know what they are
  | Pile ItemPile  -- We see a pile and know these items are on the pile
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

type ItemPile = [Item]

data Item
  = Weapon !Int    -- Weapon and its damage
  | Armor !Int     -- Armor and the AC it gives
  | Food           -- Safe food item
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

makeLenses ''Level
makeLenses ''LevelCell
makeLenses ''WorldState

emptyWorldState :: WorldState
emptyWorldState = WorldState
  { _levels = IM.empty
  , _statuses = S.empty
  , _currentLevel = 1
  , _turn = 1 }

emptyLevelCell :: LevelCell
emptyLevelCell = LevelCell
  { _cellFeature = Nothing
  , _cellItems = NoPile }

emptyLevel :: Level
emptyLevel = Level
  { _cells = A.listArray ((0, 0), (79, 21)) (repeat emptyLevelCell)
  , _boulders = S.empty
  , _monsters = M.empty }

