module GameState where
-- todo Don't expose internal functions and Data

import qualified Data.Map as M

data GameStatus = GameStatus {board :: Board, runStatus :: RunStatus}
  deriving (Show)

data RunStatus = Init | Running | Paused
  deriving (Show)

type Point = (Int, Int)
type CellMap = M.Map Point Bool
data Board = Board { boardWidth:: Int,
                     boardHeight :: Int,
                     boardCells :: CellMap,
                     currentPos :: Point}
                     deriving (Show)

emptyBoard :: Board
emptyBoard = Board {boardWidth = 60, boardHeight = 10, boardCells = M.empty, currentPos = (0,0)}

moveForward :: Board -> Board
moveForward b = moveBy b rightCell

moveBackward :: Board -> Board
moveBackward b = moveBy b leftCell

moveUp :: Board -> Board
moveUp b = moveBy b upperCell

moveDown :: Board -> Board
moveDown b = moveBy b downCell

moveBy :: Board -> (Point -> Point) -> Board
moveBy b f = b {currentPos = newPos}
  where newPos = f $ currentPos b

makeLive :: Board -> Point -> Board
makeLive b p = b {boardCells = M.insert p True bc }
  where bc = boardCells b

makeDead :: Board -> Point -> Board
makeDead b p = b {boardCells = M.insert p False bc }
  where bc = boardCells b

rightCell :: Point -> Point
rightCell (x,y) = (x+1, y)

leftCell :: Point -> Point
leftCell (x,y) = (x-1, y)

upperCell :: Point -> Point
upperCell (x,y) = (x, y-1)

downCell :: Point -> Point
downCell (x,y) = (x, y+1)

stepBoard :: Board -> Board
stepBoard b = b {boardCells = newCellMap}
  where
    cm        = boardCells b
    cs        = allPoints b
    ns        = map (countLiveNeighbors b) cs              -- live neighbor counts
    ls        = map (\p -> M.findWithDefault False p cm) cs     -- live or dead state
    newCellMap = M.fromList $ zip cs $ zipWith decideNextState ns ls

allPoints :: Board -> [Point]
allPoints b = [(x,y) | x <- [0..(w-1)], y <- [0..(h-1)]] --cells
  where w = boardWidth b
        h = boardHeight b

-- First parameter number of neighbors, second whether the cell is live or dead now
decideNextState :: Int -> Bool -> Bool
decideNextState 2 True = True -- live cell with 2 live neighbors maintains life
decideNextState 3 _ = True --live cell with 3 live neighbors maintians life, dead cell with 3 live neighbors regenerates
decideNextState _ _ = False -- any other combination remains dead

countLiveNeighbors :: Board -> Point -> Int
countLiveNeighbors b p = length $ filter id $ map (\(x,y) -> M.findWithDefault False (x,y) cm) ns
  where
    w = boardWidth b
    h = boardHeight b
    cm = boardCells b
    ns = neighbors w h p

neighbors :: Int -> Int -> Point -> [Point]
neighbors width height p =
  filter insideBound $ filter (not . negativePoint) allNs
  where
    allNs = allNeighbors p
    negativePoint (x,y) = x <0 || y<0
    insideBound (x,y) = x >=0 && x<width && y >= 0 && y<height

allNeighbors :: Point -> [Point]
allNeighbors (x, y) =
  [(x-1,y-1), (x,y-1), (x+1,y-1), -- top 3
  (x-1, y), (x+1, y),   -- beside 3
  (x-1,y+1), (x,y+1), (x+1,y+1)]  -- bottom 3
