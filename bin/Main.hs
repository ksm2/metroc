module Main(main) where

import Data.Version
import Paths_metroc (version)
import System.Environment
import System.Posix.Terminal (queryTerminal)
import System.Posix.IO (stdOutput)

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

printInfo :: IO ()
printInfo =
  do  progName <- getProgName
      primaryLn $ "Metro Compiler " ++ joinDot (versionBranch version)
      putStrLn ""

      boldLn "SYNOPSIS"
      putStrLn $ "  " ++ progName ++ " COMMAND"
      putStrLn $ "  " ++ progName ++ " [-h|--help]"
      putStrLn $ "  " ++ progName ++ " [-v|--version]"
      putStrLn ""

      boldLn "META COMMANDS"
      putStrLn $ "  " ++ progName ++ " help       Print this help text and exit."
      putStrLn $ "  " ++ progName ++ " version    Display the version number and exit."

printHelp :: IO ()
printHelp =
  do  printInfo

printVersion :: IO ()
printVersion = putStr "v" >> (putStrLn . joinDot . versionBranch) version

-- | main, do nothing
main :: IO ()
main =
  do  args <- getArgs
      case args of
        ("version":_)   -> printVersion
        ("--version":_) -> printVersion
        ("-v":_)        -> printVersion
        ("help":_)      -> printHelp
        ("--help":_)    -> printHelp
        ("-h":_)        -> printHelp
        []              -> printHelp
        _ -> build args
