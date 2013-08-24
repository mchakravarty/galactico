-- Data structures encoding the state of the world.

module World (
  Player(..), PlayerId(..), Goods(..), Futures(..), Plan(..), Action(..),
  TileIndex, Tiles, Tile(..),
  World(..), Prices(..),
  initialWorld,
  playerBounds, tileBounds,
  noGoods
) where

  -- standard library
import Data.Array
import Data.Int
import Data.Word


-- Data types describing the game state
-- ------------------------------------

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
  deriving (Show, Eq, Ord, Bounded, Enum, Ix)

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
      --        INSTEAD, we might want to store the immutable (during the game) part of the game state separately.
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


-- Common values of game state components
-- --------------------------------------

-- Initial game world.
--
initialWorld :: World
initialWorld 
  = World
    { playersW = array playerBounds [(pid, initialPlayer pid (show pid)) | pid <- range playerBounds]
    , tilesW   = initialTiles
    , storageW = noGoods
    , pricesW  = initialPrices
    }

playerBounds :: (PlayerId, PlayerId)
playerBounds = (minBound, maxBound)

initialPlayer :: PlayerId -> String -> Player
initialPlayer playerId name
  = Player
    { idP      = playerId
    , nameP    = name
    , creditsP = 100
    , goodsP   = noGoods
    , futuresP = noFutures
    , planP    = noPlan
    }

noFutures :: Futures
noFutures = Futures

noPlan :: Plan
noPlan = Plan [] []

noGoods :: Goods
noGoods = Goods 0 0 0 0

initialTiles :: Tiles
initialTiles = listArray tileBounds (repeat initialTile)

tileBounds :: (TileIndex, TileIndex)
tileBounds = ((0, 0), (7, 4))

initialTile :: Tile
initialTile
  = Tile Nothing Nothing

initialPrices :: Prices
initialPrices = Prices 10 10 10 10
