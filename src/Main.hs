module Main where

import System.Console.ANSI
import System.IO
import Control.Concurrent
import qualified Data.Map as M
import Prelude hiding (Either(..))

import GameInput
import GameState

main2 :: IO ()
main2 = do
  pause
  pause
  a <- stdin `ifReadyDo` getChar
  print a
  pause
  putStrLn "Hello"

ifReadyDo :: Handle -> IO a -> IO (Maybe a)
ifReadyDo hnd x = hReady hnd >>= f
  where f True = x >>= return . Just
        f _    = return Nothing

main :: IO ()
main = do
  let gs = GameStatus {runStatus = Init, board = emptyBoard}
  initialize
  drawGame gs
  gameLoop gs

initialize :: IO ()
initialize = do
  setTitle "Game of Life"
  setSGR [ SetConsoleIntensity BoldIntensity,
           SetColor Foreground Vivid Blue ]
  hSetEcho stdin False
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering

drawGame :: GameStatus -> IO ()
drawGame gs = do
  let b = board gs
  clearScreen
  setCursorPosition 0 0
  drawBoard b
  drawInstructions
  setCursorPosition 0 0

drawBoard :: Board -> IO ()
drawBoard b = do
  let bString = convertToString b
  mapM_ drawLine bString

drawLine :: String -> IO ()
drawLine [] = putStrLn ""
drawLine (x:xs) =
  case x of
    'o' -> do
      drawLiveCell
      drawLine xs
    '-' -> do
      drawDeadCell
      drawLine xs

drawLiveCell :: IO ()
drawLiveCell = do
  setSGR [SetColor Foreground Vivid Green]
  putStr "o"

drawDeadCell :: IO ()
drawDeadCell = do
  setSGR [SetColor Foreground Vivid Blue]
  putStr "-"

convertCell :: Maybe Bool -> Char
convertCell (Just True) = 'o'
convertCell _ = '-'

convertToString :: Board -> [String]
convertToString b = map (map (\x -> convertCell (M.lookup x cellMap))) indices
  where
    bw = boardWidth b
    bh = boardHeight b
    indices = [[(x,y) | x <- [0..(bw-1)]] | y <- [0..(bh-1)]]
    cellMap = boardCells b

drawInstructions :: IO ()
drawInstructions = do
  setSGR [ SetConsoleIntensity BoldIntensity,
           SetColor Foreground Vivid Black ]
  putStrLn "\n\nMove by using the WSAD characters"
  putStrLn "Press 'j' to liven a cell. Press 'k' to make it dead."
  putStrLn "Press 'q' to quit."

gameLoop :: GameStatus -> IO ()
gameLoop gs = do
  input <- getInput
  case input of
    Quit -> quit
    _ -> handleInput gs input

quit :: IO ()
quit = do
  clearScreen
  setCursorPosition 0 0
  putStrLn "Thanks for playing!"
  setSGR [Reset]

handleInput :: GameStatus -> Input -> IO ()
handleInput gs input = case input of
    Up ->   do
      cursorUp 1
      let updatedBoard = moveUp (board gs)
      let ngs = GameStatus {board = updatedBoard}
      gameLoop ngs
    Down -> do
      cursorDown 1
      let updatedBoard = moveDown (board gs)
      let ngs = GameStatus {board = updatedBoard}
      gameLoop ngs
    Right -> do
      cursorForward 1
      let updatedBoard = moveForward (board gs)
      let ngs = GameStatus {board = updatedBoard}
      gameLoop ngs
    Left -> do
      cursorBackward 1
      let updatedBoard = moveBackward (board gs)
      let ngs = GameStatus {board = updatedBoard}
      gameLoop ngs
    Liven -> do
      setSGR [SetColor Foreground Vivid Green]
      putStr "o"
      let updatedBoard = makeLive b curPos
      let ub = moveForward updatedBoard
      let ngs = GameStatus {board = ub}
      gameLoop ngs
      where b = board gs
            curPos = currentPos b
    Dead -> do
      setSGR [SetColor Foreground Vivid Blue]
      putStr "-"
      let updatedBoard = makeDead b curPos
      let ub = moveForward updatedBoard
      let ngs = GameStatus {board = ub}
      gameLoop ngs
      where b = board gs
            curPos = currentPos b
    Start -> do
      runSimulation gs

runSimulation :: GameStatus -> IO ()
runSimulation gs = do
  drawGame gs
  pause
  let nb = stepBoard (board gs)
  let ngs = GameStatus {board = nb}
  a <- stdin `ifReadyDo` getChar
  case a of
    (Just c) -> if c=='q' then quit else runSimulation ngs
    Nothing -> runSimulation ngs

pause :: IO ()
pause = do
  hFlush stdout
  threadDelay 400000