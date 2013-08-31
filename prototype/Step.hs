-- Advance the state of the world per animation frame (@ 30fps).

module Step (
  step
) where

  -- libraries
import Control.Lens
import Control.Monad
import Control.Monad.Trans.State
import Graphics.Gloss

  -- friends
import ViewState
import World


step :: Float -> ViewState -> IO ViewState
step timeS state = return $ execState (animationStep timeS) state


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
        { tilesV.delayS     .= 1
        ; tilesV.isVisibleS %= not
        }
    }
