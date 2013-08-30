{-# LANGUAGE TemplateHaskell, Rank2Types, NoMonomorphismRestriction #-}

-- Data structures encoding the UI state.

module ViewState (
  State(..), worldS, tilesS,
  TileSelection(..), indicesS, colourS,
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
data State 
  = State
    { _worldS :: World
    , _tilesS :: TileSelection
    }

-- A set of selected tiles and the colour used to highlight them.
--
data TileSelection
  = TileSelection
    { _indicesS :: [TileIndex]
    , _colourS  :: Color
    }

makeLenses ''State
makeLenses ''TileSelection


-- Initialisation of the view state
-- --------------------------------

-- Initial overall state
--
initialState :: State
initialState = State initialWorld emptyTileSelection

-- An empty selection
--
emptyTileSelection :: TileSelection
emptyTileSelection = TileSelection [] white
