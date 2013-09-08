-- Handle Gloss events.

module Events (
  events
) where

  -- libraries
import Control.Lens
import Control.Monad.Trans.State
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

  -- friends
import Turn
import ViewState
import World


-- Gloss event processing
-- ----------------------

type StateM = State ViewState

-- Process a single Gloss event
--
events :: Event -> StateM ()
events (EventKey (SpecialKey KeyEnter) Down   _ pos) = do
                                                       { posV .= pos
                                                       ; endOfTurn
                                                       }
events (EventKey key                   upDown _ pos) = do
                                                       { keysV.contains key .= (upDown == Down)
                                                       ; posV               .= pos
                                                       }
events _                                             = return ()
