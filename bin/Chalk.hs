module Chalk (Color (..), background, clearLine, moveUp, primaryLn, putBold, putColored, boldLn) where

import System.Posix.IO (stdOutput)
import System.Posix.Terminal (queryTerminal)

data Color
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Purple
  | Cyan
  | White
  deriving (Enum, Show)

primaryLn :: String -> IO ()
primaryLn text =
  do
    isTty <- queryTerminal stdOutput
    putStrLn $ if isTty then "\x1b[1;38;2;216;83;105m" ++ text ++ "\x1b[0m" else text

putBold :: String -> IO ()
putBold text =
  do
    isTty <- queryTerminal stdOutput
    putStr $ if isTty then "\x1b[1m" ++ text ++ "\x1b[0m" else text

boldLn :: String -> IO ()
boldLn text =
  do
    isTty <- queryTerminal stdOutput
    putStrLn $ if isTty then "\x1b[1m" ++ text ++ "\x1b[0m" else text

background :: Color -> String -> IO ()
background color text =
  do
    isTty <- queryTerminal stdOutput
    putStr $ if isTty then "\x1b[1;7;" ++ (show $ 30 + (colorToNum color)) ++ "m" ++ text ++ "\x1b[0m" else text

putColored :: Color -> String -> IO ()
putColored color text =
  do
    isTty <- queryTerminal stdOutput
    putStr $ if isTty then "\x1b[1;" ++ (show $ 30 + (colorToNum color)) ++ "m" ++ text ++ "\x1b[0m" else text

moveUp :: Int -> IO ()
moveUp n = putStr $ "\x1b[" ++ (show n) ++ "A"

clearLine :: IO ()
clearLine = putStr "\x1b[2K"

colorToNum :: Color -> Int
colorToNum Black = 0
colorToNum Red = 1
colorToNum Green = 2
colorToNum Yellow = 3
colorToNum Blue = 4
colorToNum Purple = 5
colorToNum Cyan = 6
colorToNum White = 7
