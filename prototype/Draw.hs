-- Gloss drawing functions.

module Draw (
  draw
) where

  -- standard libraries
import Data.Array
import System.IO.Unsafe (unsafePerformIO)

  -- libraries
import Control.Lens
import Graphics.Gloss

  -- friends
import ViewState
import World


-- Dimensions
-- ----------

screenWidth, screenHeight :: Float
screenWidth  = 1028
screenHeight = 768

tileSize :: Float
tileSize = 128

hudHeight :: Float
hudHeight = tileSize

playerHUDWidth :: Float
playerHUDWidth = tileSize * 2


-- Drawing
-- -------

draw :: State -> IO Picture
draw (State {_worldS = world, _tilesS = tileSel})
  = return $ Pictures 
             [ Translate 0 (hudHeight / 2)                      $ drawTiles (world^.tilesW)
             , Translate 0 tileSize $ Scale (1/8) (1/8)         mothership
             , Translate 0 (- screenHeight / 2 + hudHeight / 2) $ drawHUD (world^.turnW) (world^.playersW)
             ]

drawTiles :: Tiles -> Picture
drawTiles tiles 
  = Pictures [drawTile tidx (tiles!tidx) | tidx <- range tileBounds]

drawTile :: TileIndex -> Tile -> Picture
drawTile tidx tile
  = (uncurry Translate) (tileIndexToPoint tidx) $ 
      Pictures
      [ maybe Blank playerTint (tile^.ownerT)
      , Color gridColour $ rectangleWire tileSize tileSize
      ]
  where
    playerTint pid = Color (verytransparent $ playerColour pid) $ rectangleSolid tileSize tileSize

tileIndexToPoint :: TileIndex -> Point
tileIndexToPoint (i, j) = (fromIntegral i * tileSize - tilesWidth / 2 + 0.5, 
                           tilesHeight / 2 - fromIntegral j * tileSize + 0.5)
  where
    tilesWidth                     = tileSize * fromIntegral horMaxTile
    tilesHeight                    = tileSize * fromIntegral vertMaxTile
    (_, (horMaxTile, vertMaxTile)) = tileBounds

-- The first argument is the player whose turn it is.
--
drawHUD :: PlayerId -> Players -> Picture
drawHUD turn players 
  = Pictures 
    [ Color (greyN 0.6) $ rectangleSolid screenWidth hudHeight
    , Translate (playerHUDWidth * (-1.5)) 0 $ drawPlayerHUD turn (players!PlayerA)
    , Translate (playerHUDWidth * (-0.5)) 0 $ drawPlayerHUD turn (players!PlayerB)
    , Translate (playerHUDWidth *   0.5 ) 0 $ drawPlayerHUD turn (players!PlayerC)
    , Translate (playerHUDWidth *   1.5 ) 0 $ drawPlayerHUD turn (players!PlayerD)
    ]

-- The first argument is the player whose turn it is.
--
drawPlayerHUD :: PlayerId -> Player -> Picture
drawPlayerHUD turn (Player 
                    { _idP      = pid
                    , _nameP    = name
                    , _creditsP = credits
                    , _goodsP   = Goods 
                                  { _foodG    = food
                                  , _energyG  = energy
                                  , _metalG   = metal
                                  , _specialG = special
                                  }
                    }) 
  = Pictures 
    [ Color (transparent $ playerColour pid) $
        rectangleSolid playerHUDWidth hudHeight
    , if turn == pid then turnIndicator else Blank
    , Translate (-playerHUDWidth / 2 + 10) 30   $ Scale 0.2 0.2 $ Text ("$" ++ show credits)
    , Translate 0                          30   $ Scale 0.2 0.2 $ Text name
    , Translate (-playerHUDWidth / 2 + 10)(-10) $ Scale 0.2 0.2 $ Text ("F" ++ show food  ++ "  E" ++ show energy)
    , Translate (-playerHUDWidth / 2 + 10)(-40) $ Scale 0.2 0.2 $ Text ("M" ++ show metal ++ "  S" ++ show special)
    ]
  where
    turnIndicator = Pictures
                    [ Color black $ rectangleWire (playerHUDWidth - 0.5) (hudHeight - 0.5)
                    , Color black $ rectangleWire (playerHUDWidth - 2.5) (hudHeight - 2.5)
                    , Color black $ rectangleWire (playerHUDWidth - 4.5) (hudHeight - 4.5)
                    ]


-- Colours
-- -------

gridColour :: Color
gridColour = transparent chartreuse

playerColour :: PlayerId -> Color
playerColour PlayerA = dark azure
playerColour PlayerB = dark violet
playerColour PlayerC = dark aquamarine
playerColour PlayerD = dark orange

transparent :: Color -> Color
transparent col = makeColor r g b 0.1
  where
    (r, g, b, _a) = rgbaOfColor col

verytransparent :: Color -> Color
verytransparent col = makeColor r g b 0.05
  where
    (r, g, b, _a) = rgbaOfColor col

-- Assets
-- ------

mothership :: Picture
mothership
  = unsafePerformIO $ loadBMP "assets/Mothership.bmp"
