{-# LANGUAGE TemplateHaskell, Rank2Types, NoMonomorphismRestriction #-}

-- Data structures encoding the UI state.

module ViewState (
  ViewState(..), worldV, tilesV, keysV, posV,
  TileSelection(..), indicesS, colourS, isVisibleS, delayS, validS,
  initialState, emptyTileSelection
) where

  -- libraries
import Control.Lens
import Data.Set
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

  -- friends
import World


-- Data types describing the view state
-- ------------------------------------

-- The overall view state including the game world state.
--
data ViewState 
  = ViewState
    { _worldV :: World
    , _tilesV :: TileSelection
    , _keysV  :: Set Key          -- all currently pressed keys
    , _posV   :: Point            -- latest known mouse position
    }

-- A set of selected tiles and the colour used to highlight them.
--
data TileSelection
  = TileSelection
    { _indicesS   :: [TileIndex]
    , _colourS    :: Color
    , _isVisibleS :: Bool
    , _delayS     :: Float                             -- time until visibility gets toggled
    , _validS     :: [TileIndex] -> TileIndex -> Bool  -- whether a given tile may be added to the given selection
    }

makeLenses ''ViewState
makeLenses ''TileSelection


-- Initialisation of the view state
-- --------------------------------

-- Initial overall state
--
initialState :: ViewState
initialState = ViewState initialWorld emptyTileSelection empty (0, 0)

-- An empty selection
--
emptyTileSelection :: TileSelection
emptyTileSelection = TileSelection [] black True 0.5 (\_ _ -> True)
