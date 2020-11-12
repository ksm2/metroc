module Chalk (primaryLn, boldLn) where

import System.Posix.IO (stdOutput)
import System.Posix.Terminal (queryTerminal)

primaryLn :: String -> IO ()
primaryLn text =
  do
    isTty <- queryTerminal stdOutput
    putStrLn $ if isTty then "\x1b[1;38;2;216;83;105m" ++ text ++ "\x1b[0m" else text

boldLn :: String -> IO ()
boldLn text =
  do
    isTty <- queryTerminal stdOutput
    putStrLn $ if isTty then "\x1b[1m" ++ text ++ "\x1b[0m" else text
