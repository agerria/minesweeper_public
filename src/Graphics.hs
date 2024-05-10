module Graphics(
      screenToCell
    , fieldViewPort
    , panelViewPort
    , minesCounterViewPort
    , timeCounterViewPort
    , generateFieldPicture
    , generatePanelPicture
    , generateMinesCounterPicture
    , generateTimeCounterPicture
    , getTexturesFromBmp
) where

import Types
import Common

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

import Data.Map
import Data.Set
import Data.Tuple.Extra


--                    Constants                   --
panelNumberSize :: (Int, Int)
panelNumberSize = (26, 46)

panelWidth :: Float
panelWidth = 60

--                     Helpers                    --
screenToCell :: GameMode -> (Float, Float) -> Cell
screenToCell gm (x, y) = (nx, ny)
    where (nx, ny) = both (round . (/ cellSize)) $ invertViewPort (fieldViewPort gm) (x, y + 70)

cellToScreen :: Cell -> (Float, Float)
cellToScreen cell = (nx, ny - 70)
    where (nx, ny) = both ((* cellSize) . fromIntegral) cell

panelToScreen :: Float -> (Float, Float)
panelToScreen sw = both (/ 2) (sw, panelWidth)

minesCounterToScreen :: Int -> (Float, Float)
minesCounterToScreen n = (fromIntegral $ (* n) $ fst panelNumberSize, 0)

timeCounterToScreen :: Int -> (Float, Float)
timeCounterToScreen n = (negate $ fromIntegral $ (* n) $ fst panelNumberSize, 0)

numToArr :: Int -> [Int]
numToArr n = numToArr3 n 1
    where
    numToArr3 :: Int -> Int -> [Int]
    numToArr3 num i
        | i == 3    = [num `mod` 10]
        | otherwise = numToArr3 (num `div` 10) (i + 1) ++ [num `mod` 10]

drawEndGameSpecified :: GameTextures -> Cell -> GameState -> Picture
drawEndGameSpecified ct cell GameState{ gsField = field, gsMines = Just mines } = case Data.Map.lookup cell field of
    Just (Opened n)                    -> pictures [ (ctNumberCell ct) !! n ]
    Just Mine                          -> pictures [ ctExplodedCell ct ]
    Nothing
        | cell `Data.Set.member` mines -> pictures [ ctMineCell ct ]
        | otherwise                    -> pictures [ ctEmptyCell ct ]
    Just Flag
        | cell `Data.Set.member` mines -> pictures [ ctFlagCell ct ]
        | otherwise                    -> pictures [ ctWrongFlagCell ct ]
drawEndGameSpecified ct _ _ = pictures [ ctEmptyCell ct ]

--                   View ports                   --
fieldViewPort :: GameMode -> ViewPort
fieldViewPort gm = ViewPort (both (negate . (/ 2) . (subtract cellSize)) $ cellToScreen $ getFieldSize gm) 0 1

panelViewPort :: GameMode -> ViewPort
panelViewPort gm = ViewPort ((\(x, y) -> (negate x, y + 8)) $ both (/ 2) $ cellToScreen $ getFieldSize gm) 0 1

minesCounterViewPort :: GameMode -> ViewPort
minesCounterViewPort gm = ViewPort ((\(x, y) -> ((negate x) + (cellSize / 2) + 8, y + 8 + (panelWidth / 2))) $ both (/ 2) $ cellToScreen $ getFieldSize gm) 0 1

timeCounterViewPort :: GameMode -> ViewPort
timeCounterViewPort gm = ViewPort ((\(x, y) -> (x - (cellSize / 2) - 8, y + 8 + (panelWidth / 2))) $ both (/ 2) $ cellToScreen $ getFieldSize gm) 0 1

--               Picture generators               --
generateFieldPicture :: GameTextures -> GameState -> Picture
generateFieldPicture ct gs@GameState{ gsField = field, gsGameOver = gameOver } = pictures $ cells where
    cells = [ uncurry translate (cellToScreen (x, y)) $ drawCell x y | x <- [0 .. fieldWidth - 1], y <- [0 .. fieldHeight - 1] ]
    drawCell x y
        | gameOver          = drawEndGameSpecified ct (x, y) gs
        | otherwise         = case Data.Map.lookup (x, y) field of
            Just (Opened n) -> pictures [ (ctNumberCell ct) !! n ]
            Just Flag       -> pictures [ ctFlagCell ct ]
            _               -> pictures [ ctEmptyCell ct ]

    (fieldWidth, fieldHeight) = gsFieldSize gs

generatePanelPicture :: GameTextures -> GameState -> Picture
generatePanelPicture ct gs = pictures $ panel where
    panel      = [ uncurry translate (panelToScreen fieldWidth) $ drawPanel ]
    drawPanel  = pictures [ color (makeColor 0.502 0.502 0.502 0.502) $ rectangleSolid fieldWidth panelWidth
                          , smileTexture
                          ]
    fieldWidth = (* cellSize) $ fromIntegral $ fst $ gsFieldSize gs
    smileTexture
        | gsGameOver gs  = ctGameOverSmile ct
        | gsWin gs       = ctWinSmile ct
        | gsMouseDown gs = ctClickSmile ct
        | otherwise      = ctGoodSmile ct

generateMinesCounterPicture :: GameTextures -> GameState -> Picture
generateMinesCounterPicture ct gs = pictures $ minesCounter where
    minesCounter       = [ uncurry translate (minesCounterToScreen x) $ drawMinesCounter x | x <- [0 .. 2] ]
    drawMinesCounter x = pictures [ (ctNumberPanel ct) !! (minesCounterValues !! x)]
    minesCounterValues = numToArr $ gsMineCount gs

generateTimeCounterPicture :: GameTextures -> GameState -> Picture
generateTimeCounterPicture ct gs = pictures $ timeCounter where
    timeCounter       = [ uncurry translate (timeCounterToScreen x) $ drawTimeCounter x | x <- [0 .. 2] ]
    drawTimeCounter x = pictures [ (ctNumberPanel ct) !! (timeCounterValues !! x) ]
    timeCounterValues = reverse $ numToArr $ round $ gsTimeElapsed gs

--                 Textures reader                --
getTexturesFromBmp :: Picture -> Picture -> Picture -> GameTextures
getTexturesFromBmp allSprites allPanel allSmiles =
    GameTextures
    _emptyCell _flagCell _mineCell _explodedCell _wrongFlagCell _numberCell
    _numberPanel
    _goodSmile _clickSmile _gameOverSmile _winSmile
    where
        _emptyCell      = defSprite (0,256-16) (16,16) allSpritesData
        _flagCell       = defSprite (0,256-32) (16,16) allSpritesData
        _explodedCell   = defSprite (0,256-64) (16,16) allSpritesData
        _wrongFlagCell  = defSprite (0,256-80) (16,16) allSpritesData
        _mineCell       = defSprite (0,256-96) (16,16) allSpritesData
        _zeroCell       = defSprite (0,0)      (16,16) allSpritesData
        _oneCell        = defSprite (0,16)     (16,16) allSpritesData
        _twoCell        = defSprite (0,32)     (16,16) allSpritesData
        _threeCell      = defSprite (0,48)     (16,16) allSpritesData
        _fourCell       = defSprite (0,64)     (16,16) allSpritesData
        _fiveCell       = defSprite (0,80)     (16,16) allSpritesData
        _sixCell        = defSprite (0,96)     (16,16) allSpritesData
        _sevenCell      = defSprite (0,112)    (16,16) allSpritesData
        _eightCell      = defSprite (0,128)    (16,16) allSpritesData
        _numberCell     = [ _zeroCell
                          , _oneCell
                          , _twoCell
                          , _threeCell
                          , _fourCell
                          , _fiveCell
                          , _sixCell
                          , _sevenCell
                          , _eightCell
                          ]
        _zeroPanel      = defSprite (0,0)   (13,23) allPanelData
        _onePanel       = defSprite (0,23)  (13,23) allPanelData
        _twoPanel       = defSprite (0,46)  (13,23) allPanelData
        _threePanel     = defSprite (0,69)  (13,23) allPanelData
        _fourPanel      = defSprite (0,92)  (13,23) allPanelData
        _fivePanel      = defSprite (0,115) (13,23) allPanelData
        _sixPanel       = defSprite (0,138) (13,23) allPanelData
        _sevenPanel     = defSprite (0,161) (13,23) allPanelData
        _eightPanel     = defSprite (0,184) (13,23) allPanelData
        _ninePanel      = defSprite (0,207) (13,23) allPanelData
        _numberPanel    = [ _zeroPanel
                          , _onePanel
                          , _twoPanel
                          , _threePanel
                          , _fourPanel
                          , _fivePanel
                          , _sixPanel
                          , _sevenPanel
                          , _eightPanel
                          , _ninePanel
                          ]

        _goodSmile      = defSmile (0,0)  (24,24) allSmilesData
        _clickSmile     = defSmile (0,24) (24,24) allSmilesData
        _gameOverSmile  = defSmile (0,48) (24,24) allSmilesData
        _winSmile       = defSmile (0,72) (24,24) allSmilesData

        allSpritesData  = (\(Bitmap b) -> b) allSprites
        allPanelData    = (\(Bitmap b) -> b) allPanel
        allSmilesData   = (\(Bitmap b) -> b) allSmiles
        defSprite x y d = scale 2 2 $ BitmapSection (Rectangle {rectPos = x, rectSize = y}) d
        defSmile x y d  = scale 1.75 1.75 $ BitmapSection (Rectangle {rectPos = x, rectSize = y}) d
