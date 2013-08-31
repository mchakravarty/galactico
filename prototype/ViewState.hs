{-# LANGUAGE TemplateHaskell, Rank2Types, NoMonomorphismRestriction #-}

-- Data structures encoding the UI state.

module ViewState (
  ViewState(..), worldV, tilesV,
  TileSelection(..), indicesS, colourS, isVisibleS, delayS,
  initialState, emptyTileSelection
) where

  -- libraries
import Control.Lens
import Graphics.Gloss

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
    }

-- A set of selected tiles and the colour used to highlight them.
--
data TileSelection
  = TileSelection
    { _indicesS   :: [TileIndex]
    , _colourS    :: Color
    , _isVisibleS :: Bool
    , _delayS     :: Float          -- time until visibility gets toggled
    }

makeLenses ''ViewState
makeLenses ''TileSelection


-- Initialisation of the view state
-- --------------------------------

-- Initial overall state
--
initialState :: ViewState
initialState = ViewState initialWorld emptyTileSelection

-- An empty selection
--
emptyTileSelection :: TileSelection
emptyTileSelection = TileSelection [(0, 0)] black True 0.5
