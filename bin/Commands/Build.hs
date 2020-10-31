{-# LANGUAGE TemplateHaskell #-}
module Commands.Build(clean, build, run) where

import Data.FileEmbed
import System.Directory
import System.FilePath.Posix
import System.Process

import MetroLang.Compilation.Compile
import MetroLang.Parser as Metro
import MetroLang.WebAssembly.Generator
import MetroLang.WebAssembly.Parser as WASM
import MetroLang.WebAssembly.Utils

outDir :: String
outDir = "target"

-- | watToWasm compiles WebAssembly Text format to Binary format
watToWasm :: String -> String -> IO ()
watToWasm inFile outFile = callProcess "wat2wasm" [inFile, "-o", outFile]

-- | metroToWat compiles Metro to WebAssembly Text format
metroToWat :: String -> String -> IO ()
metroToWat inFile outFile =
  do  -- Create output directory if it is missing
      createDirectoryIfMissing True outDir

      -- Load std lib
      std <- return $ WASM.parseString $(embedStringFile "std/std.wat")

      -- Compile program and output WebAssembly Text format
      ast <- Metro.parseFile inFile
      wasm <- return $ compile ast
      generateFile outFile $ merge std wasm

-- | runWat runs a WebAssembly Text format file
runWat :: String -> IO ()
runWat inFile = callProcess "wasmtime" [inFile]

clean :: [String] -> IO ()
clean _ = removeDirectoryRecursive outDir

build :: [String] -> IO ()
build args =
  do  let inputFile:_ = args
          baseName = takeBaseName inputFile
          outWatFile = outDir </> baseName ++ ".wat"
          outWasmFile = outDir </> baseName ++ ".wasm"

      metroToWat inputFile outWatFile
      watToWasm outWatFile outWasmFile

run :: [String] -> IO ()
run args =
  do  let inputFile:_ = args
          baseName = takeBaseName inputFile
          outWatFile = outDir </> baseName ++ ".wat"

      metroToWat inputFile outWatFile
      runWat outWatFile
