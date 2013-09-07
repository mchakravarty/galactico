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
events (EventKey (SpecialKey KeyEnter) Down   _ pos) state = return
                                                               $ worldV %~ endOfTurn
                                                               $ posV   .~ pos
                                                               $ state
events (EventKey key                   upDown _ pos) state = return
                                                               $ keysV.contains key .~ (upDown == Down)
                                                               $ posV               .~ pos
                                                               $ state
events _                                             state = return state
