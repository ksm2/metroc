module Commands.Build (clean, build, run) where

import Builder.AST
import qualified Data.ByteString as B
import MetroLang.Location
import System.Directory
import System.FilePath.Posix
import Wasmtime

outDir :: String
outDir = "target"

-- | runWAT runs a WebAssembly Text format string using C bindings
runWAT :: String -> IO ()
runWAT watStr = do
  engine <- newEngine
  linker <- newLinker engine

  -- Configure WASI
  wasiConfig <- newWasiConfig
  wasiConfigInheritArgv wasiConfig
  wasiConfigInheritEnv wasiConfig
  wasiConfigInheritStdin wasiConfig
  wasiConfigInheritStdout wasiConfig
  wasiConfigInheritStderr wasiConfig

  wasm <- wat2wasm watStr
  wasmModule <- newModule engine wasm

  linkerConfigureWasi linker wasiConfig

  -- Call default function
  linkerModule linker "main-module" wasmModule
  func <- linkerGetDefault linker "main-module"
  funcCall func
  touchEngine engine

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

    let ast = metroToAST enableAssertions [(Source inputFile, inputStr)]
    let wat = astToWAT enableAssertions "main" [(Source inputFile, inputStr)] ast
    writeFile outWatFile wat

    wasm <- wat2wasm wat
    wasmB <- byteVecToByteString wasm
    B.writeFile outWasmFile wasmB

run :: [String] -> IO ()
run args =
  do
    let (enableAssertions, inputFile) = parseArgs args

    inputStr <- readFile inputFile
    let ast = metroToAST enableAssertions [(Source inputFile, inputStr)]
    let wat = astToWAT enableAssertions "main" [(Source inputFile, inputStr)] ast
    runWAT wat
