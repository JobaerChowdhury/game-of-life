module GameInput where

import Prelude hiding (Either(..))

data Input = Up | Down | Left | Right | Pause | Resume | Restart | Quit | Liven | Dead | Start
  deriving (Show, Eq)

getInput :: IO Input
getInput = do
  char <- getChar
  case char of
    'q' -> return Quit
    'p' -> return Pause
    'r' -> return Resume
    'z' -> return Restart
    'w' -> return Up
    's' -> return Down
    'a' -> return Left
    'd' -> return Right
    'j' -> return Liven
    'k' -> return Dead
    'e' -> return Start
    _ -> getInput
