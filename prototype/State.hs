module Galactico.State where

import qualified Data.Map as DM

type TUnit = Int -- counter, increment per round

type PlayerId = Int

type Position = (Int, Int)

data Terrain 
  = River
  | Field
  | Mountain
  | Desert
  | Store


data Activity
  = Mining TUnit       -- time started, output increases, then dwindles
  | Agriculture TUnit  -- time started, output dwindles over time
  | Energy             -- energy output only depends on weather
  | Inacctive

data Plot = Plot Position Terrain Activity PlayerId

data Player = Player 
  { playerId :: PlayerId
  , money:: Int
  , food :: Int
  , plotsOwned:: [Position] 
  }

data GSTate = GState 
  { time    :: TUnit
  , players :: [Player]
  , plots   :: PlotMap
}

type PlotMap = DM.Map Position Plot

startPlots = [(4,4),(4,6),(5,4),(5,6),(6,4)]

plotString 
  = ["-$~$---$~$",
     "--M$~$-$~$",
     "M--$~$-$~$",
     "-M-$~$-$~$",
     "-M-$!$-$~$",
     "-M-$~$$~$_",
     "-MM-$~$--M",
     "-M-$~$M-M-",
     "-M$~$M-M--"]

noPlayer = 0

initPlotMap = concat $ zipWith mkPos plotString [1..10]
  where 
  	mkPlot :: Char -> Position -> Plot
  	mkPlot c pos = Plot pos (toTerrain c) Inacctive noPlayer

  	mkPos :: String -> Int -> [Plot]
  	mkPos str i  = zipWith (\c -> \j -> mkPlot c (j,i)) str [1..10]

  	toTerrain c  = case c of
                    '-' -> Desert
                    '$' -> Field
                    'M' -> Mountain
                    '~' -> River
                    '!' -> Store
