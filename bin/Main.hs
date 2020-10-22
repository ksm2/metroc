module Main where

import System.Environment
import MetroLang.Parser
import MetroLang.Compile
import MetroLang.WebAssembly.Generator

-- | main, do nothing
main :: IO ()
main =
    do  args <- getArgs
        let arg1:arg2:_ = args
        ast <- parseFile arg1
        wasm <- return $ compile ast
        generateFile arg2 wasm
