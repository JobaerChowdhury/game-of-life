module Main where

import qualified Data.Map as M
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
    Up ->   do
      curUp
      let updatedBoard = moveUp (board gs)
      let ngs = GameStatus {board = updatedBoard}
      gameLoop ngs
    Down -> do
      curDown
      let updatedBoard = moveDown (board gs)
      let ngs = GameStatus {board = updatedBoard}
      gameLoop ngs
    Right -> do
      curRight
      let updatedBoard = moveForward (board gs)
      let ngs = GameStatus {board = updatedBoard}
      gameLoop ngs
    Left -> do
      curLeft
      let updatedBoard = moveBackward (board gs)
      let ngs = GameStatus {board = updatedBoard}
      gameLoop ngs
    Liven -> do
      drawLiveCell
      let updatedBoard = makeLive b curPos
      let ub = moveForward updatedBoard
      let ngs = GameStatus {board = ub}
      gameLoop ngs
      where b = board gs
            curPos = currentPos b
    Dead -> do
      drawDeadCell
      let updatedBoard = makeDead b curPos
      let ub = moveForward updatedBoard
      let ngs = GameStatus {board = ub}
      gameLoop ngs
      where b = board gs
            curPos = currentPos b
    Start -> runSimulation gs

quitWithMessage :: IO ()
quitWithMessage = quit "Thanks for playing!"

runSimulation :: GameStatus -> IO ()
runSimulation gs = do
  drawGame gs inputInstructions
  pause
  let nb = stepBoard (board gs)
  let ngs = GameStatus {board = nb}
  handleCharPress 'q' quitWithMessage (runSimulation ngs)