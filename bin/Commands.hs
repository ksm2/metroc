module Commands where

import Data.Char
import System.Environment

data Command = Build
             | Help
             | Version
               deriving Show

describeCommand :: Command -> String
describeCommand Build    = "Build the project to WebAssembly."
describeCommand Help     = "Print this help text and exit."
describeCommand Version  = "Display the version number and exit."

strPadRight :: Char -> Int -> String -> String
strPadRight c i str = if (length str) < i then strPadRight c i (str ++ [c]) else str

explainCommands :: [Command] -> IO ()
explainCommands [] = return ()
explainCommands (x:xs) =
  do  progName <- getProgName
      text <- return $ strPadRight ' ' 10 $ map toLower $ show x
      putStrLn $ "  " ++ progName ++ " " ++ text ++ " " ++ (describeCommand x)
      explainCommands xs
