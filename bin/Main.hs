{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.FileEmbed
import System.Environment

import MetroLang.Compile
import MetroLang.Parser as Metro
import MetroLang.WebAssembly.Generator
import MetroLang.WebAssembly.Parser as WASM
import MetroLang.WebAssembly.Utils

-- | main, do nothing
main :: IO ()
main =
  do  args <- getArgs
      let arg1:arg2:_ = args

      -- Load std lib
      std <- return $ WASM.parseString $(embedStringFile "std/std.wat")

      -- Compile program
      ast <- Metro.parseFile arg1
      wasm <- return $ compile ast
      generateFile arg2 $ merge std wasm
