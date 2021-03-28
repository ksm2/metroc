{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Builder.AST (metroToAST, astToWAT) where

import Data.FileEmbed
import Data.Map ((!))
import qualified Data.Map as M
import MetroLang.Compilation.Compile
import MetroLang.Lang.Error
import MetroLang.Lang.ErrorRenderer
import MetroLang.Lang.Model
import qualified MetroLang.Lang.Parser as Metro
import MetroLang.Location
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

stdModules :: M.Map Source String
stdModules =
  M.fromList
    [ ("std.metro", $(embedStringFile "std/std.metro")),
      ("Int.metro", $(embedStringFile "std/Int.metro")),
      ("UInt.metro", $(embedStringFile "std/UInt.metro")),
      ("Word.metro", $(embedStringFile "std/Word.metro")),
      ("Byte.metro", $(embedStringFile "std/Byte.metro")),
      ("File.metro", $(embedStringFile "std/File.metro"))
    ]

buildStdLib :: Bool -> Either MetroError [Module]
buildStdLib enableAssertions =
  do
    std <- Metro.parse "std.metro" (stdModules ! "std.metro")
    astInt <- Metro.parse "Int.metro" (stdModules ! "Int.metro")
    astUInt <- Metro.parse "UInt.metro" (stdModules ! "UInt.metro")
    astWord <- Metro.parse "Word.metro" (stdModules ! "Word.metro")
    astByte <- Metro.parse "Byte.metro" (stdModules ! "Byte.metro")
    astFile <- Metro.parse "File.metro" (stdModules ! "File.metro")
    let stdLib = [std, astInt, astUInt, astWord, astByte, astFile]
    if enableAssertions
      then do
        assertion <- Metro.parse "assertion.metro" $(embedStringFile "std/assertion.metro")
        return $ stdLib ++ [assertion]
      else return stdLib

-- | metroToAST compiles Metro to Metro AST
metroToAST :: Bool -> [(Source, String)] -> Module
metroToAST enableAssertions inputs =
  case go of
    Left err -> error $ renderError (M.union stdModules (M.fromList inputs)) err
    Right a -> a
  where
    go =
      do
        metroStdLib <- buildStdLib enableAssertions
        asts <- mapM (uncurry Metro.parse) inputs
        return $ foldl Metro.merge (Module []) (metroStdLib ++ asts)

-- | astToWAT compiles Metro AST to WebAssembly Text Format
astToWAT :: Bool -> String -> [(Source, String)] -> Module -> String
astToWAT enableAssertions mainMethod inputs metroModule =
  case go metroModule of
    Left err -> error $ renderError (M.union stdModules (M.fromList inputs)) err
    Right a -> a
  where
    wasmStdLib = parseWASM $(embedStringFile "std/std.wat")
    go m =
      do
        compiled <- compile enableAssertions mainMethod m
        return $ generateString $ mergeWASM wasmStdLib compiled
