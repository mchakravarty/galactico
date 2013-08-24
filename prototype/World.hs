-- Data structures encoding the state of the world.

module World (
  Player(..), PlayerId(..), Goods(..), Futures(..), Plan(..), Action(..),
  TileIndex, Tiles, Tile(..),
  World(..), Prices(..)
) where

  -- standard library
import Data.Array
import Data.Int
import Data.Word

-- Player state
--
-- The player state does not include the tiles owned by a player. Instead, the status of each tile in the tile data
-- structure includes ownership information.
--
data Player 
  = Player 
    { idP       :: PlayerId
    , nameP     :: String
    , creditsP  :: Word32
    , goodsP    :: Goods
    , futuresP  :: Futures
    , planP     :: Plan
    }
  deriving Show

-- The game comprises for players. Every round starts with 'PlayerA's turn, then 'PlayerB's, and so on.
--
data PlayerId = PlayerA | PlayerB | PlayerC | PlayerD
  deriving (Show, Eq, Ord, Enum, Ix)

-- The quantity of all available kinds of merchandise.
--
data Goods
  = Goods
    { foodG    :: Int32
    , energyG  :: Int32
    , metalG   :: Int32
    , specialG :: Int32
    }
  deriving Show

-- Futures
--
-- FIXME: implement
data Futures = Futures
  deriving Show

-- The plan of one player for one round.
--
data Plan
  = Plan
    { fieldsP :: [TileIndex]      -- Tiles that the player wishes to aquire in this order of preference.
    , routeP  :: [Action]         -- The actions that the player wishes to execute on tiles in this round.
    }
  deriving Show

-- The various actions that a player can take on a tile during the plan for one round.
--
data Action
  = Survey  TileIndex             -- Surveying of a tile. Tile must not be owned by any player.
  | Build   TileIndex Facility    -- Build a facility on a tile. The executing player must be the tile owner.
  | SellOff TileIndex             -- Sell off a facility on a tile. The player must own the tile, which must have a facility.
  deriving Show

-- The terrain is divided into 8x5 tiles.
--
type TileIndex = (Int, Int)
type Tiles     = Array TileIndex Tile

data Tile
  = Tile
    { ownerT    :: Maybe PlayerId
    , facilityT :: Maybe Facility
      -- FIXME: we need the type of tile it is (some are more suitable for some facilities than others)
    }
  deriving Show

-- Facilities that may be build on a tile.
--
-- FIXME: We probably need multiple (facility-specific) states.
--
data Facility
  = Farm
  | PowerPlant
  | Mine
  | Factory
  deriving Show

data World
  = World
    { playersW :: Array PlayerId Player
    , tilesW   :: Tiles
    , storageW :: Goods
    , pricesW  :: Prices
    }
  deriving Show

-- The price of all available kinds of merchandise.
--
data Prices
  = Prices
    { foodP    :: Word32
    , energyP  :: Word32
    , metalP   :: Word32
    , specialP :: Word32
    }
  deriving Show
