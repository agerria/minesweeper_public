module HandlerImpls(
      createMines
    , openCell
    , flagCell
) where

import Common
import Graphics(screenToCell)
import Types

import Data.Map
import Data.Set
import System.Random.Shuffle (shuffle')


--                     Helpers                    --
_getNeighbourCells :: (Int, Int) -> Cell -> [Cell]
_getNeighbourCells (fieldWidth, fieldHeight) (cx, cy) =
    [ (i, j) | i <- [cx - 1 .. cx + 1], j <- [cy - 1 .. cy + 1],
    0 <= i && i < fieldWidth, 0 <= j && j < fieldHeight ]

checkIfFlag :: Maybe CellInfo -> Bool
checkIfFlag (Just Flag) = True
checkIfFlag _           = False

isMine :: CellInfo -> Bool
isMine Mine = True
isMine _    = False

isMineInField :: Field -> Bool
isMineInField field = any isMine (Data.Map.elems field)

isOpenedCell :: CellInfo -> Bool
isOpenedCell (Opened _) = True
isOpenedCell _          = False

countOpenedCells :: Field -> Int
countOpenedCells field = Data.Map.size $ Data.Map.filter isOpenedCell field

isAllCellOpened :: GameMode -> Field -> Bool
isAllCellOpened gm field = openedCells == cellsWithoutMinesCnt
    where
        openedCells               = countOpenedCells field
        cellsWithoutMinesCnt      = fieldWidth * fieldHeight - minesCount
        (fieldWidth, fieldHeight) = getFieldSize gm
        minesCount                = getMineCount gm

--                First left click                --
createMines :: (Float, Float) -> GameState -> GameState
createMines mouse gs = gs { gsMines = Just mines }
    where
        mines                     = Data.Set.fromList $ Prelude.take mineCnt $ shuffle gen $
                                    [(i, j) | i <- [0 .. fieldWidth - 1], j <- [0 .. fieldHeight - 1], (i, j) /= cell]
        shuffle g l               = shuffle' l (fieldWidth * fieldHeight - 1) g
        cell                      = screenToCell gameMode mouse
        (fieldWidth, fieldHeight) = gsFieldSize gs
        mineCnt                   = gsMineCount gs
        gen                       = gsStdGenerator gs
        gameMode                  = gsMode gs

--            Left click to opened cell           --
checkNeighboursStateAndOpen :: (Cell -> [Cell]) -> Cell -> Field -> Mines -> Field
checkNeighboursStateAndOpen neighbourCellsGetter cell gf mines
    | neighboursWithFlag == neighboursWithMines = Prelude.foldr (_click neighbourCellsGetter mines) gf neighbourCells
    | otherwise                                 = gf
    where
        neighbourCells      = neighbourCellsGetter cell
        neighboursWithFlag  = length $ Prelude.filter (\c -> checkIfFlag $ Data.Map.lookup c gf) neighbourCells
        neighboursWithMines = length $ Prelude.filter (`Data.Set.member` mines) neighbourCells

--            Open zero cell neighbours           --
_click :: (Cell -> [Cell]) -> Mines -> Cell -> Field -> Field
_click neighbourCellsGetter mines cell field
    | cell `Data.Map.member` field = field -- do not reprocess opened cell
    | cell `Data.Set.member` mines = put Mine -- opened cell is mine
    | otherwise = let nf = put (Opened neighbours) in
        if neighbours == 0
            then Prelude.foldr (_click neighbourCellsGetter mines) nf neighbourCells -- Check neighbours
            else nf
    where
        put state      = Data.Map.insert cell state field
        neighbourCells = neighbourCellsGetter cell
        neighbours     = length $ Prelude.filter (`Data.Set.member` mines) neighbourCells

--                 Left click impl                --
click :: Cell -> GameState -> Field
click cell gs@GameState {gsField = field, gsMines = Just mines}
    | checkIfFlag cellInfoMaybe    = field
    | cell `Data.Map.member` field = checkNeighboursStateAndOpen getNeighbourCells cell field mines
    | cell `Data.Set.member` mines = put Mine -- opened cell is mine
    | otherwise = let nf = put (Opened neighbours) in
        if neighbours == 0
            then Prelude.foldr (_click getNeighbourCells mines) nf neighbourCells -- Check neighbours
            else nf
    where
        put state         = Data.Map.insert cell state field
        getNeighbourCells = _getNeighbourCells $ gsFieldSize gs
        neighbourCells    = getNeighbourCells cell
        neighbours        = length $ Prelude.filter (`Data.Set.member` mines) neighbourCells
        cellInfoMaybe     = Data.Map.lookup cell field
click _ gs = gsField gs

--                   Left click                   --
openCell :: (Float, Float) -> GameState -> GameState
openCell mouse gs@GameState {
    gsMines = Just _,
    gsGameOver = False,
    gsMode = gameMode
    } = gs {
    gsField = newField,
    gsGameOver = exploded,
    gsWin = isWin,
    gsMouseDown = False
    } where
    cell     = screenToCell gameMode mouse
    newField = click cell gs
    exploded = isMineInField newField
    isWin    = (not exploded) && (isAllCellOpened gameMode newField)
openCell _ gs = gs

--                    Right click                   --
flagCell :: (Float, Float) -> GameState -> GameState
flagCell mouse gs@GameState{ gsField = field, gsMineCount = mineCount, gsMode = gameMode } =
    case Data.Map.lookup cell field of
        Nothing   -> gs { gsField = Data.Map.insert cell Flag field , gsMineCount = mineCount - 1}
        Just Flag -> gs { gsField = Data.Map.delete cell field, gsMineCount = mineCount + 1}
        _         -> gs
        where cell = screenToCell gameMode mouse
