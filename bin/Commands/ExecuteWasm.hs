{-# LANGUAGE ForeignFunctionInterface #-}

module Commands.ExecuteWasm (runWat) where

import qualified Codec.Binary.UTF8.String as UTF8
import Foreign.C.String
import Foreign.C.Types (CChar (..), CInt (..), CSize (..))
import Foreign.Marshal.Array
import Foreign.Ptr

type WasmEngine = Ptr ()

type WasmStore = Ptr ()

type WasmModule = Ptr ()

type WasmLinker = Ptr ()

type WasmInstance = Ptr ()

foreign import ccall "runtime.h create_engine" createEngine :: IO WasmEngine

foreign import ccall "runtime.h delete_engine" deleteEngine :: WasmEngine -> IO ()

foreign import ccall "runtime.h create_store" createStore :: WasmEngine -> IO WasmStore

foreign import ccall "runtime.h delete_store" deleteStore :: WasmStore -> IO ()

foreign import ccall "runtime.h create_module" createModule :: WasmEngine -> CSize -> Ptr CChar -> IO WasmModule

foreign import ccall "runtime.h delete_module" deleteModule :: WasmModule -> IO ()

foreign import ccall "runtime.h create_linker" createLinker :: WasmStore -> IO WasmLinker

foreign import ccall "runtime.h delete_linker" deleteLinker :: WasmLinker -> IO ()

foreign import ccall "runtime.h create_instance" createInstance :: WasmLinker -> WasmModule -> IO WasmInstance

foreign import ccall "runtime.h delete_instance" deleteInstance :: WasmInstance -> IO ()

foreign import ccall "runtime.h link_wasi" linkWasi :: WasmStore -> WasmLinker -> IO ()

foreign import ccall "runtime.h call_func" callFunc :: WasmInstance -> CInt -> IO ()

foreign import ccall "runtime.h find_func_index" findFuncIndexC :: WasmModule -> CString -> IO CInt

-- | withEngine runs a callback with a WASM Engine
withEngine :: (WasmEngine -> IO a) -> IO a
withEngine cb =
  do
    engine <- createEngine
    result <- cb engine
    deleteEngine engine
    return result

-- | withStore runs a callback with a WASM Store
withStore :: WasmEngine -> (WasmStore -> IO a) -> IO a
withStore engine cb =
  do
    store <- createStore engine
    result <- cb store
    deleteStore store
    return result

-- | withModule runs a callback with a WASM Module
withModule :: WasmEngine -> [CChar] -> (WasmModule -> IO a) -> IO a
withModule engine watBin cb =
  withArrayLen watBin $ \watSize watPtr ->
    do
      wasmModule <- createModule engine (fromIntegral watSize) watPtr
      result <- cb wasmModule
      deleteModule wasmModule
      return result

-- | withLinker runs a callback with a WASM Linker
withLinker :: WasmStore -> (WasmLinker -> IO a) -> IO a
withLinker store cb =
  do
    wasmLinker <- createLinker store
    result <- cb wasmLinker
    deleteLinker wasmLinker
    return result

-- | withInstance runs a callback with a WASM Instance
withInstance :: WasmLinker -> WasmModule -> (WasmInstance -> IO a) -> IO a
withInstance wasmLinker wasmModule cb =
  do
    wasmInstance <- createInstance wasmLinker wasmModule
    result <- cb wasmInstance
    deleteInstance wasmInstance
    return result

-- | findFuncIndex finds a func export inside a module
findFuncIndex :: WasmModule -> String -> IO CInt
findFuncIndex m s = withCString s $ \cStr -> findFuncIndexC m cStr

-- | runWat runs a WebAssembly Text format string using C bindings
runWat :: String -> IO ()
runWat watStr = runWatBin $ map fromIntegral $ UTF8.encode watStr

-- | runWatBin runs a WebAssembly Text format byte array using C bindings
runWatBin :: [CChar] -> IO ()
runWatBin watBin =
  withEngine $ \engine ->
    withStore engine $ \store ->
      withModule engine watBin $ \wasmModule ->
        withLinker store $ \linker ->
          do
            linkWasi store linker
            withInstance linker wasmModule $ \wasmInstance ->
              do
                idx <- findFuncIndex wasmModule "main"
                callFunc wasmInstance idx
