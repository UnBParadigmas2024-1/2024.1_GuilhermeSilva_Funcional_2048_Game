module GUI where

import Control.Lens hiding (Empty)
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Types

tileEdgeL :: Float
tileEdgeL = 100

gapWidth :: Float
gapWidth = 10

tileRadius :: Float
tileRadius = 4

tilePrecision :: Int
tilePrecision = 20

renderScore :: Score -> Picture
renderScore s = scale 0.2 0.2 $ pictures
    [ color white $ Text "Pontos:"
    , translate 500 0 $ color white $ Text $ show s
    ]

renderRoundedRect, renderQuarterRoundedRect
    :: Int           
    -> Float       
    -> Float        
    -> Float        
    -> Picture
renderRoundedRect p w h r = pictures
    [ quarterRoundedRect
    , rotate 90  quarterRoundedRect
    , rotate 180 quarterRoundedRect
    , rotate 270 quarterRoundedRect
    ]
    where quarterRoundedRect = renderQuarterRoundedRect p w h r

renderQuarterRoundedRect p w h r = polygon $ quarterRoundedRectPath p w h r

quarterRoundedRectPath
    :: Int          
    -> Float        
    -> Float        
    -> Float        
    -> Path         
quarterRoundedRectPath p w h r =
    [(0, 0), (0, h / 2)] ++ reverse _arcPath ++ [(w / 2, 0)]
  where
    _arcPath = arcPath p (_w, _h) r
    _h       = h / 2 - r
    _w       = w / 2 - r

arcPath :: Int -> (Float, Float) -> Float -> Path
arcPath p (x, y) r = map (getPoint x y r) $ 0.0 : map
    (\v -> pi / 2 / fromIntegral v)
    (reverse [1 .. p + 1])

getPoint :: Float -> Float -> Float -> Float -> (Float, Float)
getPoint x y r th = (x + r * cos th, y + r * sin th)


renderTileText :: Tile -> Picture
renderTileText tile = case tile of
    Tile n -> renderTxt n
    _      -> pictures []
  where
    renderTxt n =
        let txt      = show n
            len      = fromIntegral $ length txt
            baseSize = 80                     
            scaleXY  = 0.52 - 0.055 * len
            transX   = -baseSize * scaleXY * len / 2
            transY   = -baseSize * scaleXY / 2
        in  translate transX transY $ scale scaleXY scaleXY $ Text txt

renderTile :: Float -> Tile -> Picture
renderTile x tile =
    let tileColor  = getTileBgColor tile
        background = color (backColor tileColor)
            $ renderRoundedRect tilePrecision tileEdgeL tileEdgeL tileRadius
        number = color (fontColor tileColor) $ renderTileText tile
    in  translate x 0 $ pictures [background, number]

renderRow :: [Tile] -> Picture
renderRow tiles = translate (-165) 0 $ pictures $ map
    (\(tile, i) -> renderTile (i * rowHeight) tile)
    (zip tiles [0, 1, 2, 3])
    where rowHeight = tileEdgeL + gapWidth

renderBoard :: Board -> Picture
renderBoard (Board tiles) = pictures $ map
    (\(tileR, i) -> translate 0 (i * rowHeight) $ renderRow tileR)
    (zip tiles [1.5, 0.5, -0.5, -1.5])
    where rowHeight = tileEdgeL + gapWidth

renderGameOverTip :: Picture
renderGameOverTip = pictures
    [ color (makeColorI 255 0 0 0) $ translate (-110) 30 $ scale 0.3 0.3 $ Text
        "Game Over"
    , translate (-160) (-30) $ scale 0.2 0.2 $ Text "Pressione espaço para reiniciar"
    , translate (-140) (-70) $ scale 0.2 0.2 $ Text "ou ESC para sair"
    ]

renderApp :: GameState -> Picture
renderApp gameState = pictures
    [ translate (-215) 250 $ renderScore $ gameState ^. score
    , renderBoard $ gameState ^. board
    , gameTip
    ]
  where
    gameTip = case gameState ^. status of
        GameOver -> renderGameOverTip
        _        -> pictures []



getTileBgColor :: Tile -> TileStyle
getTileBgColor tile = case tile of
    Empty -> TileStyle (makeColorI 205 192 180 255) white
    Tile 2 ->
        TileStyle (makeColorI 238 228 218 255) black
    Tile 4 ->
        TileStyle (makeColorI 237 224 200 255) black
    Tile 8     -> TileStyle (makeColorI 242 177 121 255) white
    Tile 16    -> TileStyle (makeColorI 245 149 99 255) white
    Tile 32    -> TileStyle (makeColorI 246 124 95 255) white
    Tile 64    -> TileStyle (makeColorI 246 94 59 255) white
    Tile 128   -> TileStyle (makeColorI 237 207 114 255) white
    Tile 256   -> TileStyle (makeColorI 237 204 97 255) white
    Tile 512   -> TileStyle (makeColorI 237 200 80 255) white
    Tile 1024  -> TileStyle (makeColorI 237 197 63 255) white
    Tile 2048  -> TileStyle (makeColorI 237 194 46 255) white
    Tile 4096  -> TileStyle (makeColorI 237 194 46 255) white
    Tile 8192  -> TileStyle (makeColorI 237 0 46 255) white
    Tile 16384 -> TileStyle (makeColorI 237 194 46 255) white
    Tile 32768 -> TileStyle (makeColorI 237 194 46 255) white
    Tile 65536 -> TileStyle (makeColorI 237 194 46 255) white
    _ -> TileStyle (makeColorI 238 228 218 90) (makeColorI 119 110 101 255)
