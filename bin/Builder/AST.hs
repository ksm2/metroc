{-# LANGUAGE TemplateHaskell #-}

module Builder.AST (metroToAST, astToWAT) where

import Data.FileEmbed
import MetroLang.Compilation.Compile
import MetroLang.Lang.Model
import qualified MetroLang.Lang.Parser as Metro
import qualified MetroLang.WebAssembly.AST
import MetroLang.WebAssembly.Generator
import qualified MetroLang.WebAssembly.Parser

parseWASM :: String -> MetroLang.WebAssembly.AST.Module
parseWASM = MetroLang.WebAssembly.Parser.parseString

mergeWASM ::
  MetroLang.WebAssembly.AST.Module ->
  MetroLang.WebAssembly.AST.Module ->
  MetroLang.WebAssembly.AST.Module
mergeWASM = MetroLang.WebAssembly.Parser.merge

buildStdLib :: Bool -> [Module]
buildStdLib enableAssertions =
  let stdLibMetro = Metro.parse "std.metro" $(embedStringFile "std/std.metro")
      intMetro = Metro.parse "Int.metro" $(embedStringFile "std/Int.metro")
      uintMetro = Metro.parse "UInt.metro" $(embedStringFile "std/UInt.metro")
      wordMetro = Metro.parse "Word.metro" $(embedStringFile "std/Word.metro")
      byteMetro = Metro.parse "Byte.metro" $(embedStringFile "std/Byte.metro")
      fileMetro = Metro.parse "File.metro" $(embedStringFile "std/File.metro")
      stdLib = [stdLibMetro, intMetro, uintMetro, wordMetro, byteMetro, fileMetro]
   in if enableAssertions
        then stdLib ++ [Metro.parse "assertion.metro" $(embedStringFile "std/assertion.metro")]
        else stdLib

-- | metroToAST compiles Metro to Metro AST
metroToAST :: Bool -> [(String, String)] -> Module
metroToAST enableAssertions inputs =
  let metroStdLib = buildStdLib enableAssertions
      asts = map (uncurry Metro.parse) inputs
   in foldl Metro.merge (Module []) $ metroStdLib ++ asts

-- | astToWAT compiles Metro AST to WebAssembly Text Format
astToWAT :: Bool -> String -> Module -> String
astToWAT enableAssertions mainMethod =
  let wasmStdLib = parseWASM $(embedStringFile "std/std.wat")
   in generateString . mergeWASM wasmStdLib . compile enableAssertions mainMethod
