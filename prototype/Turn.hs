-- Manage player turns and round-based world progress.

module Turn (
  endOfTurn
) where


import Data.List (partition, groupBy)
import Control.Monad (liftM)

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
    ; securedPlots <- resolvePlotBidding world
    ; world'  <- foldM world executeTurn securedPlots
    ; world'' <- worldStep world'
    ; return world
    }
  
-- Assign a plot to each player according to wish list, resolve conflicts
-- randomly
resolvePlotBidding :: World -> IO [(PlayerId, TileIndex)]  
resolvePlotBidding world 
  = error "resolvePlotBidding not implemented yet"
  -- apply assignPlots to world, update world accordingly
  where
  	-- return a list of players with their assigned plots, and a list of players who 
    -- lost out
    assignPlot :: [(PlayerId, TileIndex)] -> IO ([(PlayerId, TileIndex)], [PlayerId])
    assignPlot choices = do 
      { let groupChoices = groupBy (\a -> \b -> snd a == snd b) choices
      ; let (noConflicts, conflicts)  = partition ((==1) . length) groupChoices
      ; (winners, losers) <- unzipM $ mapM chooseRandom $ map (map fst) conflicts
      ; let winnerPlots = zipWith (\w -> \g -> (w, snd $ head' g)) winners conflicts
      ; return (winnerPlots, concat losers)
      }

    head' :: [(PlayerId, TileIndex)] -> (PlayerId, TileIndex)
    head' []    = error $ "Turn.resolvePlotBidding applying head to empty list, impossible"
    head' (x:_) = x

    -- choose a random player from a list of ids, return succesfull and failed players
    chooseRandom :: [PlayerId] -> IO (PlayerId, [PlayerId])
    chooseRandom (p:ps) = return (p, ps) -- todo: proper random

    assignPlots :: [(PlayerId, [TileIndex])] -> IO [(PlayerId, TileIndex)] 
    assignPlots [] = return []
    assignPlots playerPlots = do 
      { let firstChoices = map getFirstChoice playerPlots
      ; (assigns, unsuccessfullPlayers) <- assignPlot firstChoices
      ; otherAssigns <- assignPlots $ zipPlots unsuccessfullPlayers playerPlots
      ; return (assigns ++ otherAssigns)
      }

    getFirstChoice :: (PlayerId, [TileIndex]) -> (PlayerId, TileIndex)
    getFirstChoice (playerId, (p:ps)) = (playerId, p)
    getFirstChoice (playerId, []) 
      = error $  "Turn.resolvePlotBidding: player " ++ (show playerId) ++
        " has insufficient number of plot choices in plan"

    zipPlots [] _ = []
    zipPlots p@(p1:prs) ((p1',pl1'): pls) 
      = if (p1 == p1')
          then (p1, tail pl1') : (zipPlots prs pls)
          else zipPlots p pls      

executeTurn :: (PlayerId, TileIndex) -> World -> IO World
executeTurn (playerId, tileId) world 
  = do
    { putStrLn $ (show playerId) ++" buys plot " ++ (show tileId)
    ; return world
    }  

-- Apply random world events, calculate field output
worldStep :: World -> IO World
worldStep world = return world


foldM :: Monad m => b -> (a -> b -> m b) -> [a] -> m b
foldM b f [] = return b
foldM b f (x:xs) 
  = do 
  { b' <- f x b 
  ; foldM b' f xs
  }

unzipM :: Monad m => m [(a,b)] -> m ([a], [b])
unzipM = liftM unzip 
