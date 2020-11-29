module Commands.Build (clean, build, run) where

import Builder.AST
import Builder.Conversion
import Builder.Instance (callFunc, withInstance)
import Builder.Runtime (withRuntime)
import qualified Data.ByteString as B
import System.Directory
import System.FilePath.Posix

outDir :: String
outDir = "target"

-- | runWAT runs a WebAssembly Text format string using C bindings
runWAT :: String -> IO ()
runWAT watStr =
  withRuntime $ \runtime ->
    withInstance runtime watStr $ \wasmInstance ->
      callFunc wasmInstance "main"

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

    ast <- return $ metroToAST enableAssertions [(inputFile, inputStr)]
    wat <- return $ astToWAT enableAssertions "main" ast
    writeFile outWatFile wat

    wasm <- watToWasm wat
    B.writeFile outWasmFile wasm

run :: [String] -> IO ()
run args =
  do
    let (enableAssertions, inputFile) = parseArgs args

    inputStr <- readFile inputFile
    ast <- return $ metroToAST enableAssertions [(inputFile, inputStr)]
    wat <- return $ astToWAT enableAssertions "" ast
    runWAT wat
