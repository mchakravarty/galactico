-- Manage player turns and round-based world progress.

module Turn (
  endOfTurn
) where

  -- standard libraries
import Control.Applicative
import Control.Monad
import Data.Array
import Data.List
import Data.Word

  -- libraries
import Control.Lens
import Control.Monad.Trans.State

  -- friends
import World
import Prelude hiding (round)


-- Turn progression
-- ----------------

endOfTurn :: World -> World
endOfTurn world@(World {_turnW = PlayerA}) = world {_turnW = PlayerB}
endOfTurn world@(World {_turnW = PlayerB}) = world {_turnW = PlayerC}
endOfTurn world@(World {_turnW = PlayerC}) = world {_turnW = PlayerD}
endOfTurn world@(World {_turnW = PlayerD}) = execState endOfRound world {_turnW = PlayerA}


-- Round progression
-- -----------------

type WorldM = State World

-- Progress the world state by finalising the current round.
--
endOfRound :: WorldM ()
endOfRound
  = do
    { securedPlots <- resolvePlotBidding
    ; purchasePlots securedPlots
    ; executeActions
    ; worldStep
    }
  
-- Assign plots to each player, and reset fields in plan to empty list.
--
purchasePlots :: [(PlayerId, TileIndex)] -> WorldM ()
purchasePlots playerTiles
  = tilesW %= setTileOwners playerTiles
  where
    setTileOwners :: [(PlayerId, TileIndex)] -> Tiles -> Tiles  
    setTileOwners pts tiles
      = tiles // [(tId, (tiles!tId){_ownerT = Just pId}) | (pId, tId) <- pts] 
--    = tiles // [(tId, ownerT .~ Just pId $ tiles!tId) | (pId, tId) <- pts]  -- the lens way of writing the same

-- Assign a plot to each player according to their wish list; resolve conflicts randomly.
--
resolvePlotBidding :: WorldM [(PlayerId, TileIndex)]  
resolvePlotBidding
  = do 
    { players <- use playersW
    ; let choices = map (\(pId, p) -> (pId, p^.planP^.fieldsP)) $ assocs $ players
    ; assignPlots choices
    }
    where
    -- Return a list of players with their assigned plots and a list of players who 
    -- lost out.
    assignPlot :: [(PlayerId, TileIndex)] -> WorldM ([(PlayerId, TileIndex)], [PlayerId])
    assignPlot choices = do 
      { let groupChoices = groupBy (\a -> \b -> snd a == snd b) choices
      ; let (noConflicts, conflicts)  = partition ((==1) . length) groupChoices
      ; (winners, losers) <- unzip <$> (mapM chooseRandom $ map (map fst) conflicts)
      ; let winnerPlots = zipWith (\w -> \g -> (w, snd $ head' g)) winners conflicts
      ; return (winnerPlots, concat losers)
      }

    head' :: [(PlayerId, TileIndex)] -> (PlayerId, TileIndex)
    head' []    = error $ "Turn.resolvePlotBidding applying head to empty list, impossible"
    head' (x:_) = x

    -- choose a random player from a list of ids, return succesfull and failed players
    chooseRandom :: [PlayerId] -> WorldM (PlayerId, [PlayerId])
    chooseRandom (p:ps) = return (p, ps) -- todo: proper random

    assignPlots :: [(PlayerId, [TileIndex])] -> WorldM [(PlayerId, TileIndex)] 
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
      = error $ "Turn.resolvePlotBidding: player " ++ (show playerId) ++
        " has insufficient number of plot choices in plan"

    zipPlots [] _ = []
    zipPlots p@(p1:prs) ((p1',pl1'):pls) 
      | p1 == p1' = (p1, tail pl1') : (zipPlots prs pls)
      | otherwise = zipPlots p pls      

-- No actual IO action in the prototype
executeActions :: WorldM ()
executeActions
  = mapM_ executePlayerActions [PlayerA .. PlayerD]
    where
      executePlayerActions :: PlayerId -> WorldM ()
      executePlayerActions pId = do
        { players <- use playersW
        ; let actions = (players!pId)^.planP^.routeP
        ; mapM_ (executePlayerAction pId) actions
        }

      -- Notifies a player of survey results on tile.
      -- We will record this in the state, so the GUI does it asynchonously!
      executePlayerAction pId (Survey tId)
        = return () -- todo

      executePlayerAction pId (Build tId fac)
        = do 
          { cost <- facilityCost fac
          ; addCredits pId (-1 * cost)
          ; return () -- todo
          }

addCredits :: PlayerId -> Word32 -> WorldM ()
addCredits pId amount 
  = return () -- todo


-- Cost to install facility
facilityCost :: Facility -> WorldM Word32
facilityCost _ = return 0 -- todo

-- Apply random world events, and calculate field output.
--
worldStep :: WorldM ()
worldStep = return ()
