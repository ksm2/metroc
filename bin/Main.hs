module Main where

import System.Environment
import MetroLang.WebAssembly.Parser
import MetroLang.WebAssembly.Generator

-- | main, do nothing
main :: IO ()
main =
    do  args <- getArgs
        let arg1:arg2:_ = args
        ast <- parseFile arg1
        print ast
        generateFile arg2 ast
