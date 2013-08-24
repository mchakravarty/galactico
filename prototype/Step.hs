-- Advance the state of the world per animation frame (@ 30fps).

module Step (
  step
) where

  -- libraries
import Graphics.Gloss

  -- friends
import World


step :: Float -> World -> IO World
step timeS world = return world

