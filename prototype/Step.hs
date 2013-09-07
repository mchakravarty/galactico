-- Advance the state of the world per animation frame (@ 30fps).

module Step (
  step
) where

  -- libraries
import Control.Lens
import Control.Monad
import Control.Monad.Trans.State
import Data.List
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

  -- friends
import Draw
import ViewState
import World


step :: Float -> ViewState -> IO ViewState
step timeS state = return $ execState (animationStep timeS >> handleUserInput) state


-- Animation step
-- --------------

type StateM = State ViewState

-- Perform one frame of the animation.
--
animationStep :: Float -> StateM ()
animationStep timeS
  = do
    { animateTileSel timeS
    }

-- Animate the tile selection (i.e., toggle visibility every 1s).
--
animateTileSel :: Float -> StateM ()
animateTileSel timeS
  = do
    { currentDelay <- tilesV.delayS <-= timeS
    ; when (currentDelay <= 0.0) $ do
        { tilesV.delayS     .= 0.5
        ; tilesV.isVisibleS %= not
        }
    }


-- Handling of user input
-- ----------------------

-- Inspect the current state of the controls and issue the user implied actions.
--
handleUserInput :: StateM ()
handleUserInput
  = do
    { leftButtonPressed <- use $ keysV.contains (MouseButton LeftButton)
    ; when leftButtonPressed $ do
        { pos <- use posV 
        ; case uielement pos of
            Nothing         -> return ()
            Just HUD        -> return ()
            Just (Map tidx) -> selectTile Nothing tidx
        }
    ; leftButtonPressed <- use $ keysV.contains (MouseButton RightButton)
    ; when leftButtonPressed $ do
        { pos <- use posV 
        ; case uielement pos of
            Nothing         -> return ()
            Just HUD        -> return ()
            Just (Map tidx) -> deselectTile tidx
        }
    }

-- Add the given tile to the selected ones if it is a valid tile for the current selection (and isn't selected yet).
--
-- The optional first arguments determines where in the selection sequence we add the new tile.
--
selectTile :: Maybe Int -> TileIndex -> StateM ()
selectTile eidx tidx
  = do
    { state <- get
    ; let eidx' = maybe (length $ state^.tilesV^.indicesS) id eidx
    ; when (tidx `notElem` state^.tilesV^.indicesS && (state^.tilesV^.validS) tidx) $
        tilesV.indicesS %= insertAt eidx' tidx
    }

-- Remove the given tile from the sequence of selected times.
--
deselectTile :: TileIndex -> StateM ()
deselectTile tidx
  = tilesV.indicesS %= delete tidx

insertAt :: Int -> a -> [a] -> [a]
insertAt i x xs
  = let (l, r) = splitAt i xs
    in
    l ++ [x] ++ r
