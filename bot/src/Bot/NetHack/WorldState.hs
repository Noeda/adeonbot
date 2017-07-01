{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}

module Bot.NetHack.WorldState
  ( WorldState(..)
  , Turn
  , Hitpoints
  , LevelIndex
  , hasFoodInInventory
  , hasSoreLegs
  , soreLegsUntil
  , hp
  , maxHP
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
  , statues
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
  , Item(..)
  , ItemIdentity(..)
  , isFood
  , armor
  , ArmorSlot(..)
  , ArmorSpecial(..)
  , CorrosionLevel
  , EnchantmentLevel
  , unlocksDoors
  , buc
  , enchantment
  , corrosion
  , quantity
  , itemIdentity )
  where

import Control.Lens hiding ( Level, levels, (.=) )
import Data.Aeson
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
type Hitpoints = Int
type CorrosionLevel = Int
type EnchantmentLevel = Int

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
  , _soreLegsUntil  :: !Turn
  , _hp             :: !Hitpoints
  , _maxHP          :: !Hitpoints
  , _turn           :: !Turn }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, FromJSON, ToJSON )

data LevelMeta = LevelMeta
  { _levelDescription :: !T.Text
  , _branchName       :: !T.Text }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, FromJSON, ToJSON )

data Level = Level
  { _cells    :: !(A.Array (Int, Int) LevelCell)
  , _whereSearchedLastTime :: !(Maybe (Int, Int, Int))
  , _statues :: !(S.Set (Int, Int))
  , _boulders :: !(S.Set (Int, Int))
  , _monsters :: !(M.Map (Int, Int) MonsterImage) }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

instance ToJSON Level where
  toJSON lvl = object
    [ "cells" .= (A.bounds (_cells lvl), A.assocs (_cells lvl))
    , "whereSearchedLastTime" .= (_whereSearchedLastTime lvl)
    , "statues" .= (_statues lvl)
    , "boulders" .= (_boulders lvl)
    , "monsters" .= (_monsters lvl) ]

instance FromJSON Level where
  parseJSON (Object ob) = do
    (bounds, cells) <- ob .: "cells"
    searched_last_time <- ob .: "whereSearchedLastTime"
    statues <- ob .: "statues"
    boulders <- ob .: "boulders"
    monsters <- ob .: "monsters"
    return Level
      { _cells = A.array bounds cells
      , _whereSearchedLastTime = searched_last_time
      , _statues = statues
      , _boulders = boulders
      , _monsters = monsters }

  parseJSON _ = fail "FromJSON.Level: not an object"

data LevelCell = LevelCell
  { _cellFeature :: !(Maybe LevelFeature)
  , _numberOfTimesSearched :: !Int
  , _cellItemAppearanceLastTime :: String
  , _cellItems :: !ItemPileImage }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, FromJSON, ToJSON )

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
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum, FromJSON, ToJSON )

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
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum, FromJSON, ToJSON )

data BUC
  = Uncursed
  | Blessed
  | Cursed
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum, FromJSON, ToJSON )

data Status
  = Confstunned            -- Confused or stunned. Right now we treat them the same.
  | Blind                  -- Blind. Can't infer things about surroundings the same way.
  | FoodPoisoning          -- Oh my, what did you eat??? BAD BOT
  | Hungry                 -- Hungry, Weak and Fainting all in one
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum, FromJSON, ToJSON )

data ItemPileImage
  = NoPile         -- We don't see any item pile here
  | PileSeen       -- We see a pile of items but don't know what they are
  | Pile ItemPile  -- We see a pile and know these items are on the pile
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, FromJSON, ToJSON )

type ItemPile = [Item]

data Item = Item
  { _itemIdentity :: !ItemIdentity
  , _enchantment :: !(Maybe EnchantmentLevel)
  , _corrosion :: !CorrosionLevel
  , _quantity :: !Int
  , _buc :: !(Maybe BUC) }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, FromJSON, ToJSON )

data ArmorSlot
  = Helmet
  | Gloves
  | Body
  | Shoes
  | TShirt
  | Shield
  | Cloak
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum, FromJSON, ToJSON )

data ArmorSpecial
  = MagicResistance
  | Reflection
  | NoSpecials
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum, FromJSON, ToJSON )

armor :: Int -> ArmorSlot -> ItemIdentity
armor ac slot = Armor ac slot NoSpecials

data ItemIdentity
  = Weapon !Int    -- Weapon; damage and base damage
  | Armor !Int !ArmorSlot !ArmorSpecial
  | Food           -- Safe food item
  | PickAxe        -- Pick-axe, nice murder weapon
  | Key            -- Can lock and unlock doors
  | CreditCard     -- Can unlock doors
  | MagicMarker    -- Nice!
  | NonTonalInstrument    -- drums
  | ScaryTonalInstrument  -- tooled horn
  | Statue
  | StrangeItem    -- No idea what this item is.
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, FromJSON, ToJSON )

unlocksDoors :: ItemIdentity -> Bool
unlocksDoors Key = True
unlocksDoors CreditCard = True
unlocksDoors _ = False

data MonsterImage = MonsterImage
  { _monster :: !Monster
  , _isPeaceful :: !(Maybe Bool)
  , _monsterAppearance :: String }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, FromJSON, ToJSON )
makeLenses ''MonsterImage
makeLenses ''Level
makeLenses ''LevelCell
makeLenses ''Item
makeLenses ''WorldState
makeLenses ''LevelMeta

emptyWorldState :: WorldState
emptyWorldState = WorldState
  { _levels = IM.empty
  , _levelMeta = IM.empty
  , _statuses = S.empty
  , _currentLevel = 1
  , _soreLegsUntil = 1
  , _hp = 1
  , _maxHP = 1
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
  , _statues = S.empty
  , _monsters = M.empty }

hasStatue :: Level -> (Int, Int) -> Bool
hasStatue lvl pos = fromMaybe False (do
  itemimage <- lvl^?cells.ix pos.cellItems
  return $ case itemimage of
    Pile items -> any ((== Statue) . _itemIdentity) items
    _ -> False) ||

  pos `S.member` (lvl^.statues)

isFood :: Item -> Bool
isFood (_itemIdentity -> Food) = True
isFood _ = False

hasFoodInInventory :: WorldState -> Bool
hasFoodInInventory wstate = any isFood (wstate^.inventory)

hasSoreLegs :: WorldState -> Bool
hasSoreLegs wstate = wstate^.soreLegsUntil >= wstate^.turn

