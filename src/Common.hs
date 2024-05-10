module Common(
      cellSize
    , getFieldSize
    , getWindowSize
    , getMineCount
) where

import Types

import Data.Tuple.Extra


--                    Constants                   --
cellSize :: Float
cellSize = 32

--                     Helpers                    --
getFieldSize :: GameMode -> (Int, Int)
getFieldSize gm = case gm of
    Beginner     -> (8, 8)   -- 10 mines
    Amateur      -> (16, 16) -- 40 mines
    Professional -> (30, 16) -- 99 mines

getWindowSize :: GameMode -> (Int, Int)
getWindowSize gm = (nx + 20, ny + 90)
    where (nx, ny) = both (* (round cellSize)) (getFieldSize gm)

getMineCount :: GameMode -> Int
getMineCount gm = case gm of
    Beginner     -> 10
    Amateur      -> 40
    Professional -> 99
