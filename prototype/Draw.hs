-- Gloss drawing functions.

module Draw (
  draw
) where

  -- standard libraries
import Data.Array
import System.IO.Unsafe (unsafePerformIO)

  -- libraries
import Graphics.Gloss

  -- friends
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


-- Drawing
-- -------

draw :: World -> IO Picture
draw world 
  = return $ Pictures 
             [ Translate 0 (hudHeight / 2)                      $ drawTiles (tilesW world)
             , Translate 0 tileSize $ Scale (1/8) (1/8)         mothership
             , Translate 0 (- screenHeight / 2 + hudHeight / 2) drawHUD
             ]

drawTiles :: Tiles -> Picture
drawTiles tiles 
  = Pictures [drawTile tidx (tiles!tidx) | tidx <- range tileBounds]

drawTile :: TileIndex -> Tile -> Picture
drawTile tidx tile
  = (uncurry Translate) (tileIndexToPoint tidx) $ 
      Pictures
      [ maybe Blank playerTint (ownerT tile)
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

drawHUD :: Picture
drawHUD = Color (greyN 0.3) $ rectangleSolid screenWidth hudHeight


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
