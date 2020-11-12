module Main (main) where

import Commands
import Data.Char
import Data.List
import System.Environment

startsWith :: (Eq a) => a -> [a] -> Bool
startsWith _ [] = False
startsWith y (x : _) = x == y

lcFirst :: String -> String
lcFirst [] = []
lcFirst (x : xs) = (toLower x) : xs

strToCommand :: String -> Maybe Command
strToCommand str = find (\cmd -> str == (lcFirst $ show cmd)) commands

parseArgs :: [String] -> ([String], Maybe Command, [String])
parseArgs [] = ([], Nothing, [])
parseArgs (arg : args) =
  if startsWith '-' arg
    then
      let (options, command, rest) = parseArgs args
       in (arg : options, command, rest)
    else ([], strToCommand arg, args)

-- | main, do nothing
main :: IO ()
main =
  do
    rawArgs <- getArgs
    let (options, command, rest) = parseArgs rawArgs
    case options of
      ("--version" : _) -> runCommand (Just Version) rest
      ("-v" : _) -> runCommand (Just Version) rest
      _ -> runCommand command rest
