-- Gloss drawing functions.

module Draw (
  draw, 
  UIElement(..), uielement
) where

  -- standard libraries
import Data.Array
import Data.List
import Data.Maybe
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

draw :: ViewState -> IO Picture
draw (ViewState {_worldV = world, _tilesV = tileSel})
  = return $ Pictures 
             [ Translate 0 (hudHeight / 2)                      $ drawTiles (world^.tilesW) tileSel
             , Translate 0 tileSize $ Scale (1/8) (1/8)         mothership
             , Translate 0 (- screenHeight / 2 + hudHeight / 2) $ drawHUD (world^.turnW) (world^.playersW)
             ]

drawTiles :: Tiles -> TileSelection -> Picture
drawTiles tiles tileSel
  = Pictures [drawTile tidx (tiles!tidx) | tidx <- range tileBounds]
  where
    drawTile :: TileIndex -> Tile -> Picture
    drawTile tidx tile
      = (uncurry Translate) (tileIndexToPoint tidx) $ 
          Pictures
          [ maybe Blank playerTint (tile^.ownerT)
          , if tileSel^.isVisibleS && isJust eidx then selTint (tileSel^.colourS) else Blank
          , Color gridColour $ rectangleWire tileSize tileSize
          ]
      where
        eidx = tidx `elemIndex` (tileSel^.indicesS)
        
        playerTint pid = Color (veryTransparent $ playerColour pid) $ rectangleSolid tileSize tileSize
        selTint    col = Pictures 
                         [ Color (veryTransparent col) $ rectangleSolid tileSize tileSize
                         , Color (mostlyOpaque col)    $ 
                             Translate (- tileSize / 2 + 5) (- tileSize / 2 + 5) $
                               Scale 0.2 0.2 $
                                 Text $ show (fromJust eidx + 1)
                         ]

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


-- Interactions
-- ------------

-- UI elements in the interface.
--
data UIElement = HUD
               | Map TileIndex

-- Determine the UI element at the given position.
--
uielement :: Point -> Maybe UIElement
uielement (x, y)
  | y >= screenHeight / 2 || y < -screenHeight / 2 
  = Nothing
  | y < (- screenHeight / 2 + hudHeight)
  = Just HUD
  | otherwise
  = Just $ Map $ pointToTileIndex (x, y - hudHeight / 2)


-- Layout
-- ------

-- The extent of the tiled map.
--
tilesWidth, tilesHeight :: Float
(tilesWidth, tilesHeight) = (tileSize * fromIntegral horMaxTile, tileSize * fromIntegral vertMaxTile)
  where
    (_, (horMaxTile, vertMaxTile)) = tileBounds

-- Determine the location of a tile on the map.
--
tileIndexToPoint :: TileIndex -> Point
tileIndexToPoint (i, j) = (fromIntegral i * tileSize - tilesWidth / 2 + 0.5, 
                           tilesHeight / 2 - fromIntegral j * tileSize + 0.5)
  where
    tilesWidth                     = tileSize * fromIntegral horMaxTile
    tilesHeight                    = tileSize * fromIntegral vertMaxTile
    (_, (horMaxTile, vertMaxTile)) = tileBounds

-- Determine the tile at a particular location.
--
pointToTileIndex :: Point -> TileIndex
pointToTileIndex (x, y) = (round $   (x - 0.5 + tilesWidth  / 2) / tileSize,
                           round $ - (y - 0.5 - tilesHeight / 2) / tileSize)


-- Colours
-- -------

gridColour :: Color
gridColour = transparent chartreuse

playerColour :: PlayerId -> Color
playerColour PlayerA = dark azure
playerColour PlayerB = dark violet
playerColour PlayerC = dark aquamarine
playerColour PlayerD = dark orange

mostlyOpaque :: Color -> Color
mostlyOpaque col = makeColor r g b 0.7
  where
    (r, g, b, _a) = rgbaOfColor col

transparent :: Color -> Color
transparent col = makeColor r g b 0.1
  where
    (r, g, b, _a) = rgbaOfColor col

veryTransparent :: Color -> Color
veryTransparent col = makeColor r g b 0.05
  where
    (r, g, b, _a) = rgbaOfColor col

-- Assets
-- ------

mothership :: Picture
mothership
  = unsafePerformIO $ loadBMP "assets/Mothership.bmp"
