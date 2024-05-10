module Main (main) where

import GameEngine (startGame)
import Graphics(getTexturesFromBmp)
import Types

import Graphics.Gloss.Data.Bitmap

import System.Console.ANSI (clearScreen)
import System.IO (hFlush, stdout)
import System.Random (getStdGen)


chooseGameMode :: IO GameMode
chooseGameMode = do
    putStrLn "Выберите уровень сложности:"
    putStrLn "1 - Новичок (8x8, 10 мин)"
    putStrLn "2 - Любитель (16x16, 40 мин)"
    putStrLn "3 - Профи (30x16, 99 мин)"
    putStr "Ваш выбор: "
    hFlush stdout
    choice <- getLine
    case choice of
        "1" -> return Beginner
        "2" -> return Amateur
        "3" -> return Professional
        _   -> do
              clearScreen
              putStrLn "Некорректный выбор. Пожалуйста, выберите 1, 2 или 3."
              chooseGameMode

main :: IO ()
main = do
    allSprites <- loadBMP "textures/cells.bmp"
    allPanel   <- loadBMP "textures/panel.bmp"
    allSmiles   <- loadBMP "textures/smile.bmp"
    let gameTextures = getTexturesFromBmp allSprites allPanel allSmiles

    clearScreen
    mode       <- chooseGameMode
    putStrLn $ "Выбран режим: " ++ show mode

    gen        <- getStdGen -- random generator
    startGame gameTextures gen mode
