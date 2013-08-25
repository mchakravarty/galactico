-- Manage player turns and round-based world progress.

module Turn (
  round
) where

  -- friends
import World


-- Progress the world state by finalising the current round.
--
round :: World -> IO World
round world 
  = do
    { putStrLn "next round"
    ; return world
    }

