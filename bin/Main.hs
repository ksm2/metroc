module Main(main) where

import Data.Version (versionBranch)
import Paths_metroc (version)
import System.Environment
import System.Posix.Terminal (queryTerminal)
import System.Posix.IO (stdOutput)

import Commands
import Commands.Build

joinDot :: (Show a) => [a] -> String
joinDot [] = ""
joinDot [element] = show element
joinDot (x:xs) = (show x) ++ "." ++ (joinDot xs)

primaryLn :: String -> IO ()
primaryLn text =
  do  isTty <- queryTerminal stdOutput
      putStrLn $ if isTty then "\x1b[1;38;2;216;83;105m" ++ text ++ "\x1b[0m" else text

boldLn :: String -> IO ()
boldLn text =
  do  isTty <- queryTerminal stdOutput
      putStrLn $ if isTty then "\x1b[1m" ++ text ++ "\x1b[0m" else text

printHelp :: IO ()
printHelp =
  do  progName <- getProgName
      primaryLn $ "Metro Compiler " ++ joinDot (versionBranch version)
      putStrLn ""

      boldLn "SYNOPSIS"
      putStrLn $ "  " ++ progName ++ " COMMAND"
      putStrLn $ "  " ++ progName ++ " [-h|--help]"
      putStrLn $ "  " ++ progName ++ " [-v|--version]"
      putStrLn ""

      boldLn "COMPILER COMMANDS"
      explainCommands [Build]
      putStrLn ""

      boldLn "META COMMANDS"
      explainCommands [Help, Version]

printVersion :: IO ()
printVersion = putStr "v" >> (putStrLn . joinDot . versionBranch) version

startsWith :: (Eq a) => a -> [a] -> Bool
startsWith _ [] = False
startsWith y (x:_) = x == y

parseArgs :: [String] -> ([String], String, [String])
parseArgs [] = ([], "", [])
parseArgs (arg:args) =
  if startsWith '-' arg then
    let (options, command, rest) = parseArgs args
    in  (arg:options, command, rest)
  else ([], arg, args)

-- | main, do nothing
main :: IO ()
main =
  do  rawArgs <- getArgs
      let (options, command, rest) = parseArgs rawArgs
      case options of
        ("--version":_) -> printVersion
        ("-v":_)        -> printVersion
        _               -> (
          case command of
            "version" -> printVersion
            "help"    -> printHelp
            ""        -> printHelp
            "build"   -> build rest
            _         -> (
              do  putStrLn $ "Invalid command: " ++ command
                  putStrLn ""
                  printHelp
              )
          )
