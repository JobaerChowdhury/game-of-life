module GameRenderer where

import System.Console.ANSI
import System.IO
import qualified Data.Map as M
import GameState

initializePainter :: String -> IO ()
initializePainter title = do
  setTitle title
  setSGR [ SetConsoleIntensity BoldIntensity,
           SetColor Foreground Vivid Blue ]
  hSetEcho stdin False
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering

drawGame :: GameStatus -> [String] -> IO ()
drawGame gs ins = do
  let b = board gs
  clearScreen
  setCursorPosition 0 0
  drawBoard b
  drawInstructions ins
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
  setSGR [SetColor Foreground Dull Green]
  putStr "o"

drawDeadCell :: IO ()
drawDeadCell = do
  setSGR [SetColor Foreground Dull Blue]
  putStr "-"

curUp :: IO ()
curUp = cursorUp 1

curDown :: IO ()
curDown = cursorDown 1

curRight :: IO ()
curRight = cursorForward 1

curLeft :: IO ()
curLeft = cursorBackward 1

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

drawInstructions :: [String] -> IO ()
drawInstructions is = do
  setSGR [ SetConsoleIntensity BoldIntensity,
           SetColor Foreground Vivid Black ]
  mapM_ putStrLn is

quit :: String -> IO ()
quit m = do
  clearScreen
  setCursorPosition 0 0
  putStrLn m
  setSGR [Reset]