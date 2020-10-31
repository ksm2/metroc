{-# LANGUAGE TemplateHaskell #-}
module Commands.Build(clean, build) where

import Data.FileEmbed
import System.Directory
import System.FilePath.Posix
import System.Process

import MetroLang.Compilation.Compile
import MetroLang.Parser as Metro
import MetroLang.WebAssembly.Generator
import MetroLang.WebAssembly.Parser as WASM
import MetroLang.WebAssembly.Utils

-- | watToWasm compiles WebAssembly Text format to Binary format
watToWasm :: String -> String -> IO ()
watToWasm inFile outFile = callProcess "wat2wasm" [inFile, "-o", outFile]

clean :: [String] -> IO ()
clean _ =
  do  let outDir = "target"
      removeDirectoryRecursive outDir

build :: [String] -> IO ()
build args =
  do  let outDir = "target"
          inputFile:_ = args
          baseName = takeBaseName inputFile
          outWatFile = outDir </> baseName ++ ".wat"
          outWasmFile = outDir </> baseName ++ ".wasm"

      -- Create output directory if it is missing
      createDirectoryIfMissing True outDir

      -- Load std lib
      std <- return $ WASM.parseString $(embedStringFile "std/std.wat")

      -- Compile program and output WebAssembly Text format
      ast <- Metro.parseFile inputFile
      wasm <- return $ compile ast
      generateFile outWatFile $ merge std wasm

      -- Compile WebAssembly Text format to WebAssembly Binary format
      watToWasm outWatFile outWasmFile
