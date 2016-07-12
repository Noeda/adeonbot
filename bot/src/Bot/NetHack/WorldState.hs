{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.NetHack.WorldState
  ( WorldState(..)
  , Turn
  , LevelIndex
  , emptyWorldState
  , levels
  , levelMeta
  , statuses
  , currentLevel
  , inventory
  , inventoryDirty
  , turn
  , LevelMeta(..)
  , levelDescription
  , branchName
  , Level(..)
  , whereSearchedLastTime
  , hasStatue
  , emptyLevel
  , cells
  , boulders
  , monsters
  , LevelFeature(..)
  , isPassable
  , numberOfTimesSearched
  , LevelCell(..)
  , cellFeature
  , cellItems
  , cellItemAppearanceLastTime
  , BUC(..)
  , Monster(..)
  , MonsterImage(..)
  , isPeaceful
  , monster
  , monsterAppearance
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
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import GHC.Generics

type LevelIndex = Int
type Turn = Int

-- | This data represents what the bot thinks the world state is.
--
-- Kept in easily serializable form so bot's view of the world can be easily
-- saved.
data WorldState = WorldState
  { _levels         :: !(IM.IntMap Level)
  , _levelMeta      :: !(IM.IntMap LevelMeta)
  , _currentLevel   :: !LevelIndex
  , _statuses       :: !(S.Set Status)
  , _inventory      :: ![Item]
  , _inventoryDirty :: !Bool
  , _turn           :: !Turn }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

data LevelMeta = LevelMeta
  { _levelDescription :: !T.Text
  , _branchName       :: !T.Text }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

data Level = Level
  { _cells    :: !(A.Array (Int, Int) LevelCell)
  , _whereSearchedLastTime :: !(Maybe (Int, Int, Int))
  , _boulders :: !(S.Set (Int, Int))
  , _monsters :: !(M.Map (Int, Int) MonsterImage) }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

data LevelCell = LevelCell
  { _cellFeature :: !(Maybe LevelFeature)
  , _numberOfTimesSearched :: !Int
  , _cellItemAppearanceLastTime :: String
  , _cellItems :: !ItemPileImage }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

data LevelFeature
  = Wall                   -- Any wall like thing, can be black rock or | or -
  | Floor                  -- Floor things
  | Downstairs
  | Upstairs
  | Fountain               -- For all your quaffing needs
  | ClosedDoor
  | LockedDoor
  | OpenedDoor
  | Altar
  | Trap
  | Lava
  | Water
  | InkyBlackness
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

-- | Can I walk on it?
isPassable :: LevelFeature -> Bool
isPassable Floor = True
isPassable Downstairs = True
isPassable Upstairs = True
isPassable ClosedDoor = False
isPassable OpenedDoor = True
isPassable Altar = True
isPassable Trap = False
isPassable Lava = False
isPassable Water = False
isPassable Wall = False
isPassable Fountain = True
isPassable LockedDoor = False
isPassable InkyBlackness = False

data Monster
  = AmbiguousMonster       -- We see a monster here but don't know what type
                           -- it is exactly. Happens with purple 'h' monstly,
                           -- don't know if it's dwarf king of mind flayer.
  | UnremarkableMonster    -- Moves towards you and hits you. Most monsters are of this type.
  | FloatingEyeMonster     -- If you hit it directly, you get frozen.
  | MindFlayerMonster      -- Evil stuff.
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

data BUC
  = Uncursed
  | Blessed
  | Cursed
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

data Status
  = Confstunned            -- Confused or stunned. Right now we treat them the same.
  | Blind                  -- Blind. Can't infer things about surroundings the same way.
  | FoodPoisoning          -- Oh my, what did you eat??? BAD BOT
  | Hungry                 -- Hungry, Weak and Fainting all in one
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
  | Statue
  | StrangeItem    -- No idea what this item is.
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

data MonsterImage = MonsterImage
  { _monster :: !Monster
  , _isPeaceful :: !(Maybe Bool)
  , _monsterAppearance :: String }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )
makeLenses ''MonsterImage
makeLenses ''Level
makeLenses ''LevelCell
makeLenses ''WorldState
makeLenses ''LevelMeta

emptyWorldState :: WorldState
emptyWorldState = WorldState
  { _levels = IM.empty
  , _levelMeta = IM.empty
  , _statuses = S.empty
  , _currentLevel = 1
  , _inventory = []
  , _inventoryDirty = True
  , _turn = 1 }

emptyLevelCell :: LevelCell
emptyLevelCell = LevelCell
  { _cellFeature = Nothing
  , _cellItemAppearanceLastTime = ""
  , _numberOfTimesSearched = 0
  , _cellItems = NoPile }

emptyLevel :: Level
emptyLevel = Level
  { _cells = A.listArray ((0, 0), (79, 21)) (repeat emptyLevelCell)
  , _whereSearchedLastTime = Nothing
  , _boulders = S.empty
  , _monsters = M.empty }

hasStatue :: Level -> (Int, Int) -> Bool
hasStatue lvl pos = fromMaybe False $ do
  itemimage <- lvl^?cells.ix pos.cellItems
  return $ case itemimage of
    Pile items -> any (== Statue) items
    _ -> False

