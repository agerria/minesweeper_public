module GameEngine (startGame) where

import Common
import Graphics
import HandlerImpls
import Types

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

import Data.Map
import System.Random


----------------------------------------------------
--            Common functions for play           --
----------------------------------------------------

createField :: Field
createField = Data.Map.empty

gameInit :: StdGen -> GameMode -> GameState
gameInit gen gm = GameState createField (getFieldSize gm) (Nothing) (getMineCount gm) gm False 0 gen False False

renderer :: GameTextures -> GameState -> Picture
renderer ct gs@GameState{ gsMode = gameMode } = pictures $ [fieldPicture] ++ [panelPicture] ++ [minesCounterPicture] ++ [timeCounterPicture]
    where
        fieldPicture        = applyViewPortToPicture (fieldViewPort gameMode) $ generateFieldPicture ct gs
        panelPicture        = applyViewPortToPicture (panelViewPort gameMode) $ generatePanelPicture ct gs
        minesCounterPicture = applyViewPortToPicture (minesCounterViewPort gameMode) $ generateMinesCounterPicture ct gs
        timeCounterPicture  = applyViewPortToPicture (timeCounterViewPort gameMode) $ generateTimeCounterPicture ct gs

handler :: Event -> GameState -> GameState
handler (EventKey (MouseButton LeftButton) Down _ mouse) gs = gs { gsMouseDown = True }
handler (EventKey (MouseButton LeftButton) Up _ mouse) gs@GameState{ gsMines = mines, gsGameOver = False, gsWin = False } = case mines of
    Nothing -> openCell mouse $ createMines mouse gs
    Just _  -> openCell mouse gs
handler (EventKey (MouseButton RightButton) Down _ mouse) gs@GameState{ gsGameOver = False, gsWin = False } = flagCell mouse gs
handler (EventKey (Char 'r') Down _ _) GameState{ gsMode = gameMode, gsStdGenerator = gen } = gameInit gen gameMode
handler _ gs = gs


updater :: Float -> GameState -> GameState
updater deltaTime gs@GameState { gsTimeElapsed = timeElapsed, gsMines = Just _ , gsGameOver = False, gsWin = False } = gs { gsTimeElapsed = timeElapsed + deltaTime }
updater _ gs = gs

----------------------------------------------------

startGame :: GameTextures -> StdGen -> GameMode -> IO ()
startGame gameTextures gen gm = play (InWindow "Minesweeper" (getWindowSize gm) (100, 100)) (makeColor 0.753 0.753 0.753 0.753) 30 (gameInit gen gm) (renderer gameTextures) handler updater
