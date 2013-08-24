-- Handle Gloss events.

module Events (
  events
) where

  -- libraries
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

  -- friends
import World


events :: Event -> World -> IO World
events _ world = return world
