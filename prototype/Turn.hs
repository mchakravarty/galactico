-- Manage player turns and round-based world progress.

module Turn (
  endOfTurn
) where

  -- friends
import World


-- Turn progression
-- ----------------

endOfTurn :: World -> IO World
endOfTurn world@(World {turnW = PlayerA}) = return $ world {turnW = PlayerB}
endOfTurn world@(World {turnW = PlayerB}) = return $ world {turnW = PlayerC}
endOfTurn world@(World {turnW = PlayerC}) = return $ world {turnW = PlayerD}
endOfTurn world@(World {turnW = PlayerD}) 
  = do
    { newWorld <- Turn.round world 
    ; return $ newWorld {turnW = PlayerA}
    }


-- Round progression
-- -----------------

-- Progress the world state by finalising the current round.
--
round :: World -> IO World
round world 
  = do
    { putStrLn "next round"
    ; return world
    }

