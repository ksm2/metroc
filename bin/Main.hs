module Main where

import System.Environment
import MetroLang.WebAssembly.Parser

-- | main, do nothing
main :: IO ()
main =
    do  args <- getArgs
        let arg1:_ = args
        ast <- parseFile arg1
        print ast
