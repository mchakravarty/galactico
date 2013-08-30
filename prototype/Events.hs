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


events :: Event -> State -> IO State
events (EventKey (SpecialKey KeyEnter) Down _ _) state = return $ worldS %~ endOfTurn $ state
events _                                         state = return state
