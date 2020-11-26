{-# LANGUAGE TemplateHaskell #-}

module Commands.Build (clean, build, run) where

import Commands.ExecuteWasm
import qualified Data.ByteString as B
import Data.FileEmbed
import MetroLang.AST
import MetroLang.Compilation.Compile
import MetroLang.Parser as Metro
import MetroLang.WebAssembly.Generator
import MetroLang.WebAssembly.Parser as WASM
import System.Directory
import System.FilePath.Posix

outDir :: String
outDir = "target"

buildStdLib :: Bool -> [Module]
buildStdLib enableAssertions =
  let stdLibMetro = Metro.parseString "std.metro" $(embedStringFile "std/std.metro")
      intMetro = Metro.parseString "Int.metro" $(embedStringFile "std/Int.metro")
      uintMetro = Metro.parseString "UInt.metro" $(embedStringFile "std/UInt.metro")
      wordMetro = Metro.parseString "Word.metro" $(embedStringFile "std/Word.metro")
      byteMetro = Metro.parseString "Byte.metro" $(embedStringFile "std/Byte.metro")
      fileMetro = Metro.parseString "File.metro" $(embedStringFile "std/File.metro")
      stdLib = [stdLibMetro, intMetro, uintMetro, wordMetro, byteMetro, fileMetro]
   in if enableAssertions
        then stdLib ++ [Metro.parseString "assertion.metro" $(embedStringFile "std/assertion.metro")]
        else stdLib

-- | metroToWat compiles Metro to WebAssembly Text format
metroToWat :: Bool -> String -> String -> String -> String
metroToWat enableAssertions mainMethod inputFile inputStr =
  let wasmStdLib = WASM.parseString $(embedStringFile "std/std.wat")
      metroStdLib = buildStdLib enableAssertions
      ast = Metro.parseString inputFile inputStr
      wasm = compile enableAssertions mainMethod $ foldl Metro.merge (Mod []) $ metroStdLib ++ [ast]
   in generateString $ WASM.merge wasmStdLib wasm

parseArgs :: [String] -> (Bool, String)
parseArgs ["--assertions", x] = (True, x)
parseArgs [x] = (False, x)
parseArgs [] = error "Please provide an input file."
parseArgs _ = error "Too many arguments."

clean :: [String] -> IO ()
clean _ = removeDirectoryRecursive outDir

build :: [String] -> IO ()
build args =
  do
    let (enableAssertions, inputFile) = parseArgs args
        baseName = takeBaseName inputFile
        outWatFile = outDir </> baseName ++ ".wat"
        outWasmFile = outDir </> baseName ++ ".wasm"

    createDirectoryIfMissing True outDir
    inputStr <- readFile inputFile

    wat <- return $ metroToWat enableAssertions "main" inputFile inputStr
    writeFile outWatFile wat

    wasm <- watToWasm wat
    B.writeFile outWasmFile wasm

run :: [String] -> IO ()
run args =
  do
    let (enableAssertions, inputFile) = parseArgs args

    inputStr <- readFile inputFile
    wat <- return $ metroToWat enableAssertions "" inputFile inputStr
    runWat wat
