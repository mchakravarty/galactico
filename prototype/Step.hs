-- Advance the state of the world per animation frame (@ 30fps).

module Step (
  step
) where

  -- libraries
import Graphics.Gloss

  -- friends
import ViewState
import World


step :: Float -> State -> IO State
step timeS state = return state

