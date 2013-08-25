-- Manage player turns and round-based world progress.

module Turn (
  endOfTurn
) where

  -- friends
import World
import Prelude hiding (round)

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
    ; world' <- foldM world turn [PlayerA .. PlayerD]
    ; return world
    }
  

turn :: PlayerId -> World -> IO World
turn playerId world 
  = do
    { putStrLn $ (show playerId) ++"'s turn"
    ; return world
    }  



foldM :: Monad m => b -> (a -> b -> m b) -> [a] -> m b
foldM b f [] = return b
foldM b f (x:xs) 
  = do 
  { b' <- f x b 
  ; foldM b' f xs
  }
