  -- libraries
import Control.Monad.Trans.State
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

  -- friends
import Draw
import Events
import ViewState
import ViewState
import Step
import Turn
import World

main :: IO ()
main 
  = playIO
      (InWindow "Galactico" (1024, 768) (100, 100))
      white
      30
      initialiseGame
      draw
      handleEvents
      step
  where
    initialiseGame     = execState (startTurnOf PlayerA) initialState
    handleEvents event = return . execState (events event)
