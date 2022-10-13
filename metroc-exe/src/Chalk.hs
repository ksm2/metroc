module Chalk (Color (..), background, clearLine, moveUp, primaryLn, putBold, putColored, putColoredBold, boldLn) where

import Tty

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
  putTtyLn text $ "\x1b[1;38;2;216;83;105m" ++ text ++ "\x1b[0m"

putBold :: String -> IO ()
putBold text =
  putTty text $ "\x1b[1m" ++ text ++ "\x1b[0m"

boldLn :: String -> IO ()
boldLn text =
  putTtyLn text $ "\x1b[1m" ++ text ++ "\x1b[0m"

background :: Color -> String -> IO ()
background color text =
  putTty text $ "\x1b[1;7;" ++ show (30 + colorToNum color) ++ "m" ++ text ++ "\x1b[0m"

putColored :: Color -> String -> IO ()
putColored color text =
  putTty text $ "\x1b[" ++ show (30 + colorToNum color) ++ "m" ++ text ++ "\x1b[0m"

putColoredBold :: Color -> String -> IO ()
putColoredBold color text =
  putTty text $ "\x1b[1;" ++ show (30 + colorToNum color) ++ "m" ++ text ++ "\x1b[0m"

moveUp :: Int -> IO ()
moveUp n =
  ifTty $ putStr $ "\x1b[" ++ show n ++ "A"

clearLine :: IO ()
clearLine =
  ifTty $ putStr "\x1b[2K"

colorToNum :: Color -> Int
colorToNum Black = 0
colorToNum Red = 1
colorToNum Green = 2
colorToNum Yellow = 3
colorToNum Blue = 4
colorToNum Purple = 5
colorToNum Cyan = 6
colorToNum White = 7
