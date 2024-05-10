module Types(
    Cell
  , CellInfo(..)
  , Field
  , Mines
  , GameState (..)
  , GameMode(..)
  , GameTextures(..)
) where

import Graphics.Gloss

import Data.Map
import Data.Set
import System.Random


type Cell = (Int, Int)

data CellInfo = Opened Int | Mine | Flag

type Field = Map Cell CellInfo
type Mines = Set Cell

data GameMode = Beginner | Amateur | Professional deriving (Show)

data GameState = GameState {
    gsField           :: Field
  , gsFieldSize       :: (Int, Int)
  , gsMines           :: Maybe Mines
  , gsMineCount       :: Int
  , gsMode            :: GameMode
  , gsGameOver        :: Bool
  , gsTimeElapsed     :: Float
  , gsStdGenerator    :: StdGen
  , gsWin             :: Bool
  , gsMouseDown       :: Bool
}

data GameTextures = GameTextures {
    ctEmptyCell     :: Picture
  , ctFlagCell      :: Picture
  , ctMineCell      :: Picture
  , ctExplodedCell  :: Picture
  , ctWrongFlagCell :: Picture
  , ctNumberCell    :: [Picture]
  , ctNumberPanel   :: [Picture]
  , ctGoodSmile     :: Picture
  , ctClickSmile    :: Picture
  , ctGameOverSmile :: Picture
  , ctWinSmile      :: Picture
}
