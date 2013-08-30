  -- packages
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

  -- friends
import Draw
import Events
import ViewState
import ViewState
import Step
import World

main :: IO ()
main 
  = playIO
      (InWindow "Galactico" (1024, 768) (100, 100))
      white
      30
      initialState
      draw
      events
      step
