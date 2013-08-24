-- Gloss drawing functions.

module Draw (
  draw
) where

  -- libraries
import Graphics.Gloss

  -- friends
import World


draw :: World -> IO Picture
draw world = return Blank
