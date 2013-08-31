-- Handle Gloss events.

module Events (
  events
) where

  -- libraries
import Control.Lens
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

  -- friends
import Turn
import ViewState
import World


events :: Event -> ViewState -> IO ViewState
events (EventKey (SpecialKey KeyEnter) Down _ _) state = return $ worldV %~ endOfTurn $ state
events _                                         state = return state
