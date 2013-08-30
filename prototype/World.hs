{-# LANGUAGE TemplateHaskell, Rank2Types, NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

-- Data structures encoding the state of the game world.

module World (
  Player(..), idP, nameP, creditsP, goodsP, futuresP, planP,
  PlayerId(..), 
  Goods(..), foodG, energyG, metalG, specialG,
  Futures(..), 
  Plan(..), fieldsP, routeP,
  PlayerAction(..),
  TileIndex, Tiles, Tile(..), ownerT, facilityT, geoPropertyT,
  Facility (..),
  GeoProperty(..), geographyE, foodE, energyE, metalE, specialE,
  Players,
  World(..), playersW, turnW, tilesW, storageW, pricesW,
  Prices(..), foodP, energyP, metalP, specialP,
  NaturalEvent(..), NaturalEventKind(..), NaturalEventEffect, kindN, foodN, energyN, metalN, specialN,
  initialWorld,
  playerBounds, tileBounds,
  noGoods
) where

  -- standard library
import Data.Array
import Data.Int
import Data.Word

  -- libraries
import Control.Lens

  
-- Data types describing the game state
-- ------------------------------------

-- Player state
--
-- The player state does not include the tiles owned by a player. Instead, the status of each tile in the tile data
-- structure includes ownership information.
--
data Player 
  = Player 
    { _idP       :: PlayerId
    , _nameP     :: String
    , _creditsP  :: Word32
    , _goodsP    :: Goods
    , _futuresP  :: Futures
    , _planP     :: Plan
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
    { _foodG    :: Int32
    , _energyG  :: Int32
    , _metalG   :: Int32
    , _specialG :: Int32
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
    { _fieldsP :: [TileIndex]      -- Tiles that the player wishes to aquire in this order of preference.
    , _routeP  :: [PlayerAction]   -- The actions that the player wishes to execute on tiles in this round.
    }
  deriving Show

-- The various actions that a player can take on a tile during the plan for one round.
--
data PlayerAction
  = Survey  TileIndex             -- Surveying of a tile. Tile must not be owned by any player.
  | Build   TileIndex Facility    -- Build a facility on a tile. The executing player must be the tile owner.
  | SellOff TileIndex             -- Sell off a facility on a tile. The player must own the tile, which must have a facility.
  deriving Show

-- The terrain is divided into 8x5 tiles.
--
type TileIndex = (Int, Int)             -- (0, 0) is the upper left corner
type Tiles     = Array TileIndex Tile

data Tile
  = Tile
    { _ownerT       :: Maybe PlayerId
    , _facilityT    :: Maybe Facility
    , _geoPropertyT :: GeoProperty
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

-- Geographic properties of tiles.
--
data GeoProperty 
  = GeoProperty 
    { _geographyE :: Geography
    , _foodE      :: YieldFactor
    , _energyE    :: YieldFactor
    , _metalE     :: YieldFactor
    , _specialE   :: YieldFactor
    }
  deriving (Show)

type YieldFactor = Int -- 0 to 100

data Geography = River | Mountain | Meadow | Desert deriving (Show)

-- All players
--
type Players = Array PlayerId Player
  
data World
  = World
    { _playersW :: Players
    , _turnW    :: PlayerId
    , _tilesW   :: Tiles
    , _storageW :: Goods
    , _pricesW  :: Prices
    }
  deriving Show

-- The price of all available kinds of merchandise.
--
data Prices
  = Prices
    { _foodP    :: Word32
    , _energyP  :: Word32
    , _metalP   :: Word32
    , _specialP :: Word32
    }
  deriving Show

-- Natural events that affect the production of goods.
--
data NaturalEvent 
  = NaturalEvent 
    { _kindN    :: NaturalEventKind
    , _foodN    :: NaturalEventEffect
    , _energyN  :: NaturalEventEffect
    , _metalN   :: NaturalEventEffect
    , _specialN :: NaturalEventEffect
    }
  deriving Show

data NaturalEventKind = Flood | EarthQuake deriving Show -- ....

-- Compute the factor by which a tiles yield changes in dependencs on a natural event and the tiles location.
--
type NaturalEventEffect =  World -> TileIndex -> YieldFactor -> YieldFactor   

instance Show NaturalEventEffect where
  show _ = "<natural event effect>"

-- Make lenses for all record types.
--
makeLenses ''Player
makeLenses ''Goods
makeLenses ''Futures
makeLenses ''Plan
makeLenses ''Tile
makeLenses ''GeoProperty
makeLenses ''World
makeLenses ''Prices
makeLenses ''NaturalEvent


-- Common values of game state components
-- --------------------------------------

-- Initial game world.
--
initialWorld :: World
initialWorld 
  = World
    { _playersW = array playerBounds [(pid, initialPlayer pid (show pid)) | pid <- range playerBounds]
    , _turnW    = PlayerA
    , _tilesW   = initialTiles
    , _storageW = noGoods
    , _pricesW  = initialPrices
    }

playerBounds :: (PlayerId, PlayerId)
playerBounds = (minBound, maxBound)

initialPlayer :: PlayerId -> String -> Player
initialPlayer playerId name
  = Player
    { _idP      = playerId
    , _nameP    = name
    , _creditsP = 100
    , _goodsP   = noGoods
    , _futuresP = noFutures
    , _planP    = noPlan
    }

noFutures :: Futures
noFutures = Futures

noPlan :: Plan
noPlan = Plan [] []

noGoods :: Goods
noGoods = Goods 0 0 0 0

-- todo: proper geography
initialTiles :: Tiles
initialTiles = listArray tileBounds (replicate 16 initialTile ++ 
                                     [playerATile, playerCTile] ++ 
                                     replicate 3 initialTile ++ 
                                     [playerBTile, playerDTile] ++ 
                                     repeat initialTile)
  where
    playerATile = ownerT .~ (Just PlayerA) $ initialTile
    playerBTile = ownerT .~ (Just PlayerB) $ initialTile
    playerCTile = ownerT .~ (Just PlayerC) $ initialTile
    playerDTile = ownerT .~ (Just PlayerD) $ initialTile

tileBounds :: (TileIndex, TileIndex)
tileBounds = ((0, 0), (7, 4))

initialTile :: Tile
initialTile
  = Tile Nothing Nothing (GeoProperty Meadow 0 0 0 0)

initialPrices :: Prices
initialPrices = Prices 10 10 10 10
