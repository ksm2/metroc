{-# LANGUAGE TemplateHaskell #-}

module Builder.AST (metroToAST, astToWAT) where

import Data.FileEmbed
import MetroLang.AST
import MetroLang.Compilation.Compile
import qualified MetroLang.Parser
import qualified MetroLang.WebAssembly.AST
import MetroLang.WebAssembly.Generator
import qualified MetroLang.WebAssembly.Parser

parseMetro :: String -> String -> Module
parseMetro = MetroLang.Parser.parseString

mergeMetro :: Module -> Module -> Module
mergeMetro = MetroLang.Parser.merge

parseWASM :: String -> MetroLang.WebAssembly.AST.Module
parseWASM = MetroLang.WebAssembly.Parser.parseString

mergeWASM ::
  MetroLang.WebAssembly.AST.Module ->
  MetroLang.WebAssembly.AST.Module ->
  MetroLang.WebAssembly.AST.Module
mergeWASM = MetroLang.WebAssembly.Parser.merge

buildStdLib :: Bool -> [Module]
buildStdLib enableAssertions =
  let stdLibMetro = parseMetro "std.metro" $(embedStringFile "std/std.metro")
      intMetro = parseMetro "Int.metro" $(embedStringFile "std/Int.metro")
      uintMetro = parseMetro "UInt.metro" $(embedStringFile "std/UInt.metro")
      wordMetro = parseMetro "Word.metro" $(embedStringFile "std/Word.metro")
      byteMetro = parseMetro "Byte.metro" $(embedStringFile "std/Byte.metro")
      fileMetro = parseMetro "File.metro" $(embedStringFile "std/File.metro")
      stdLib = [stdLibMetro, intMetro, uintMetro, wordMetro, byteMetro, fileMetro]
   in if enableAssertions
        then stdLib ++ [parseMetro "assertion.metro" $(embedStringFile "std/assertion.metro")]
        else stdLib

-- | metroToAST compiles Metro to Metro AST
metroToAST :: Bool -> [(String, String)] -> Module
metroToAST enableAssertions inputs =
  let metroStdLib = buildStdLib enableAssertions
      asts = map (uncurry parseMetro) inputs
   in foldl mergeMetro (Mod []) $ metroStdLib ++ asts

-- | astToWAT compiles Metro AST to WebAssembly Text Format
astToWAT :: Bool -> String -> Module -> String
astToWAT enableAssertions mainMethod =
  let wasmStdLib = parseWASM $(embedStringFile "std/std.wat")
   in generateString . (mergeWASM wasmStdLib) . (compile enableAssertions mainMethod)
