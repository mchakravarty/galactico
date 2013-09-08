-- Manage player turns and round-based world progress.

module Turn (
  endOfTurn, startTurnOf
) where

  -- standard libraries
import Control.Applicative
import Control.Monad
import Data.Array
import Data.List
import Data.Word
import qualified Debug.Trace as Trace

  -- libraries
import Control.Lens
import Control.Monad.Trans.State

  -- friends
import Draw
import ViewState
import World
import Prelude hiding (round)


-- Turn progression
-- ----------------

type StateM = State ViewState

-- A player finished their turn. If it was the last player in a round, finish the round. 
--
endOfTurn :: StateM ()
endOfTurn
  = do
    { state <- get 
    
        -- The current tile selection becomes the current player's bidding plan.
    ; worldV.playersW.player(state^.worldV^.turnW).planP.fieldsP .= state^.tilesV^.indicesS
    
        -- If the current player is the last player of that round, finalise the round.
    ; when (state^.worldV.turnW == PlayerD) $
        worldV %= execState endOfRound
        
        -- Determine the player whose turn is next, and start that turn.
    ; newTurn <- worldV.turnW <%= nextPlayer
    ; startTurnOf newTurn
    }
  where
    nextPlayer PlayerA = PlayerB
    nextPlayer PlayerB = PlayerC
    nextPlayer PlayerC = PlayerD
    nextPlayer PlayerD = PlayerA
    
-- Set the view state and world up for a new turn by the given player.
--
startTurnOf :: PlayerId -> StateM ()
startTurnOf newTurn
  = startBiddingTileSelection newTurn

-- Set up the view state for the tile bidding selection of the given player.
--
startBiddingTileSelection :: PlayerId -> StateM ()
startBiddingTileSelection pid
  = do
    { state <- get
    ; tilesV          .= emptyTileSelection 
    ; tilesV.indicesS .= state^.worldV^.playersW^.player(state^.worldV^.turnW).planP.fieldsP
    ; tilesV.colourS  .= playerColour pid
    ; tilesV.validS   .= biddingTilesForPlayer [tidx | (tidx, Tile {_ownerT = Just _}) <- assocs $ state^.worldV^.tilesW]
                                               -- tiles with an owner are off limits
    }
  where
    biddingTilesForPlayer offLimits tiles tix = length tiles < 4 && tix `notElem` offLimits


-- Round progression
-- -----------------

type WorldM = State World

-- Progress the world state by finalising the current round.
--
endOfRound :: WorldM ()
endOfRound
  = do
    { trace "Plot bidding..."
    ; securedPlots <- resolvePlotBidding
    ; trace $ "...bidding result: " ++ show securedPlots
    ; purchasePlots securedPlots
    ; executeActions
    ; worldStep
    }
  
-- Assign plots to each player, and reset fields in plan to empty list.
--
purchasePlots :: [(PlayerId, TileIndex)] -> WorldM ()
purchasePlots playerTiles
  = do
    { tilesW %= setTileOwners playerTiles
    ; mapMOf_ (traverse._2) removeTile playerTiles
    }
  where
    setTileOwners :: [(PlayerId, TileIndex)] -> Tiles -> Tiles  
    setTileOwners pts tiles
      = tiles // [(tId, (tiles!tId){_ownerT = Just pId}) | (pId, tId) <- pts] 
--    = tiles // [(tId, ownerT .~ Just pId $ tiles!tId) | (pId, tId) <- pts]  -- the lens way of writing the same

    removeTile tidx = playersW %= (traverse %~ remove)
      where
        remove = planP.fieldsP %~ filter (/= tidx)

-- Assign a plot to each player according to their wish list; resolve conflicts randomly.
--
resolvePlotBidding :: WorldM [(PlayerId, TileIndex)]  
resolvePlotBidding
  = do 
    { players <- use playersW
    ; let choices = over (mapped._2) (\p -> p^.planP^.fieldsP) $ assocs $ players
    ; assignPlots choices
    }
    where
    -- Return a list of players with their assigned plots and a list of players who 
    -- lost out.
    assignPlot :: [(PlayerId, TileIndex)] -> WorldM ([(PlayerId, TileIndex)], [PlayerId])
    assignPlot choices = do 
      { let groupChoices = groupBy (\a b -> snd a == snd b) . sortBy (\a b -> snd a `compare` snd b) $ choices
            (noConflicts, conflicts) = partition ((==1) . length) groupChoices
      ; (winners, losers) <- unzip <$> (mapM chooseRandom $ map (map fst) conflicts)
      ; let winnerPlots = zipWith (\w g -> (w, snd $ head' g)) winners conflicts
      ; return (concat noConflicts ++ winnerPlots, concat losers)
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


-- Debugging
-- ---------

trace :: String -> WorldM ()
trace msg = Trace.trace msg $ return ()

traceShow :: Show a => a -> WorldM ()
traceShow v = Trace.traceShow v $ return ()
