{-# LANGUAGE TemplateHaskell #-}

module Commands.Build (clean, build, run) where

import Data.FileEmbed
import MetroLang.AST
import MetroLang.Compilation.Compile
import MetroLang.Parser as Metro
import MetroLang.WebAssembly.Generator
import MetroLang.WebAssembly.Parser as WASM
import System.Directory
import System.FilePath.Posix
import System.Process

outDir :: String
outDir = "target"

-- | watToWasm compiles WebAssembly Text format to Binary format
watToWasm :: String -> String -> IO ()
watToWasm inFile outFile = callProcess "wat2wasm" [inFile, "-o", outFile]

buildStdLib :: Bool -> [Module]
buildStdLib enableAssertions =
  let intMetro = Metro.parseString "Int.metro" $(embedStringFile "std/Int.metro")
      uintMetro = Metro.parseString "UInt.metro" $(embedStringFile "std/UInt.metro")
      wordMetro = Metro.parseString "Word.metro" $(embedStringFile "std/Word.metro")
      byteMetro = Metro.parseString "Byte.metro" $(embedStringFile "std/Byte.metro")
      fileMetro = Metro.parseString "File.metro" $(embedStringFile "std/File.metro")
      stdLib = [intMetro, uintMetro, wordMetro, byteMetro, fileMetro]
   in if enableAssertions
        then stdLib ++ [Metro.parseString "assertion.metro" $(embedStringFile "std/assertion.metro")]
        else stdLib

-- | metroToWat compiles Metro to WebAssembly Text format
metroToWat :: Bool -> String -> String -> IO ()
metroToWat enableAssertions inFile outFile =
  do
    -- Create output directory if it is missing
    createDirectoryIfMissing True outDir

    -- Load std lib
    stdWasm <- return $ WASM.parseString $(embedStringFile "std/std.wat")
    stdMetro <- return $ Metro.parseString "std.metro" $(embedStringFile "std/std.metro")
    stdLib <- return $ buildStdLib enableAssertions

    -- Compile program and output WebAssembly Text format
    ast <- Metro.parseFile inFile
    wasm <- return $ compile enableAssertions $ foldl Metro.merge stdMetro $ stdLib ++ [ast]
    generateFile outFile $ WASM.merge stdWasm wasm

-- | runWat runs a WebAssembly Text format file
runWat :: String -> IO ()
runWat inFile = callProcess "wasmtime" [inFile]

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

    metroToWat enableAssertions inputFile outWatFile
    watToWasm outWatFile outWasmFile

run :: [String] -> IO ()
run args =
  do
    let (enableAssertions, inputFile) = parseArgs args
        baseName = takeBaseName inputFile
        outWatFile = outDir </> baseName ++ ".wat"

    metroToWat enableAssertions inputFile outWatFile
    runWat outWatFile
