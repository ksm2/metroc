module Commands where

import Chalk
import Commands.Build
import Commands.Test
import Commands.Version
import Data.Char
import System.Environment

data Command
  = Clean
  | Build
  | Run
  | Test
  | Help
  | Version
  deriving (Bounded, Enum, Show)

commands :: [Command]
commands = [minBound .. maxBound]

printHelp :: [String] -> IO ()
printHelp _ =
  do
    progName <- getProgName
    primaryLn $ "Metro Compiler " ++ getVersion
    putStrLn ""

    boldLn "SYNOPSIS"
    putStrLn $ "  " ++ progName ++ " COMMAND"
    putStrLn $ "  " ++ progName ++ " [-h|--help]"
    putStrLn $ "  " ++ progName ++ " [-v|--version]"
    putStrLn ""

    boldLn "COMPILER COMMANDS"
    explainCommands [Clean, Build, Run, Test]
    putStrLn ""

    boldLn "META COMMANDS"
    explainCommands [Help, Version]

describeCommand :: Command -> String
describeCommand Clean = "Remove the target directory."
describeCommand Build = "Build the project to WebAssembly."
describeCommand Run = "Run the project main function."
describeCommand Test = "Executes all project tests."
describeCommand Help = "Print this help text and exit."
describeCommand Version = "Display the version number and exit."

runCommand :: Maybe Command -> [String] -> IO ()
runCommand (Just Clean) = clean
runCommand (Just Build) = build
runCommand (Just Run) = run
runCommand (Just Test) = test
runCommand (Just Help) = printHelp
runCommand (Just Version) = printVersion
runCommand Nothing = invalidCommand

invalidCommand :: [String] -> IO ()
invalidCommand args =
  do
    putStrLn "Invalid command"
    putStrLn ""
    printHelp args

strPadRight :: Char -> Int -> String -> String
strPadRight c i str = if length str < i then strPadRight c i (str ++ [c]) else str

explainCommands :: [Command] -> IO ()
explainCommands [] = return ()
explainCommands (x : xs) =
  do
    progName <- getProgName
    let text = strPadRight ' ' 10 $ map toLower $ show x
    putStrLn $ "  " ++ progName ++ " " ++ text ++ " " ++ describeCommand x
    explainCommands xs
