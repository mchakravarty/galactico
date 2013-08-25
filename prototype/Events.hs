-- Handle Gloss events.

module Events (
  events
) where

  -- libraries
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

  -- friends
import Turn
import World


events :: Event -> World -> IO World
events (EventKey (SpecialKey KeyEnter) Down _ _) world = endOfTurn world
events _                                         world = return world
