module Main where

import Prelude hiding (Either(..))

import GameInput
import GameState
import GameRenderer

main :: IO ()
main = do
  let gs = GameStatus {runStatus = Init, board = emptyBoard}
  initializePainter "Game of Life"
  drawGame gs inputInstructions
  gameLoop gs

gameLoop :: GameStatus -> IO ()
gameLoop gs = do
  input <- getInput
  case input of
    Quit -> quitWithMessage
    _ -> handleInput gs input

handleInput :: GameStatus -> Input -> IO ()
handleInput gs input = case input of
    Up      -> upAction gs
    Down    -> downAction gs
    Right   -> rightAction gs
    Left    -> leftAction gs
    Liven   -> livenAction gs
    Dead    -> deadAction gs
    Start   -> runSimulation gs
    _       -> gameLoop gs

upAction :: GameStatus -> IO ()
upAction gs = do
  curUp
  let updatedBoard = moveUp (board gs)
  let ngs = gs {board = updatedBoard}
  gameLoop ngs

downAction :: GameStatus -> IO ()
downAction gs = do
  curDown
  let updatedBoard = moveDown (board gs)
  let ngs = gs {board = updatedBoard}
  gameLoop ngs

rightAction :: GameStatus -> IO ()
rightAction gs = do
  curRight
  let updatedBoard = moveForward (board gs)
  let ngs = gs {board = updatedBoard}
  gameLoop ngs

leftAction :: GameStatus -> IO ()
leftAction gs = do
  curLeft
  let updatedBoard = moveBackward (board gs)
  let ngs = gs {board = updatedBoard}
  gameLoop ngs

livenAction :: GameStatus -> IO ()
livenAction gs = do
  drawLiveCell
  let updatedBoard = makeLive b curPos
  let ub = moveForward updatedBoard
  let ngs = gs {board = ub}
  gameLoop ngs
  where b = board gs
        curPos = currentPos b

deadAction :: GameStatus -> IO ()
deadAction gs = do
  drawDeadCell
  let updatedBoard = makeDead b curPos
  let ub = moveForward updatedBoard
  let ngs = gs {board = ub}
  gameLoop ngs
  where b = board gs
        curPos = currentPos b

quitWithMessage :: IO ()
quitWithMessage = quit "Thanks for playing!!"

runSimulation :: GameStatus -> IO ()
runSimulation gs = do
  drawGame gs inputInstructions
  pause
  let nb = stepBoard (board gs)
  let ngs = gs {board = nb}
  handleCharPress 'q' quitWithMessage (runSimulation ngs)