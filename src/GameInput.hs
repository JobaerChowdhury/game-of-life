module GameInput where

import System.IO
import Control.Concurrent
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

ifReadyDo :: Handle -> IO a -> IO (Maybe a)
ifReadyDo hnd x = hReady hnd >>= f
  where f True = x >>= return . Just
        f _    = return Nothing

pause :: IO ()
pause = do
  hFlush stdout
  threadDelay 400000

handleCharPress :: Char -> IO () -> IO () -> IO ()
handleCharPress ch action altAction = do
  a <- stdin `ifReadyDo` getChar
  case a of
    (Just c) -> if c=='q' then action else altAction
    Nothing -> altAction
