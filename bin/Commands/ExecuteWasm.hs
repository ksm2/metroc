{-# LANGUAGE ForeignFunctionInterface #-}

module Commands.ExecuteWasm (runWat) where

import Foreign.C.String
import Foreign.Ptr

type WasmEngine = Ptr ()

type WasmStore = Ptr ()

foreign import ccall "runtime.h create_engine" createEngine :: IO WasmEngine

foreign import ccall "runtime.h delete_engine" deleteEngine :: WasmEngine -> IO ()

foreign import ccall "runtime.h create_store" createStore :: WasmEngine -> IO WasmStore

foreign import ccall "runtime.h delete_store" deleteStore :: WasmStore -> IO ()

foreign import ccall "runtime.h run_wat_file" runWatFile :: WasmEngine -> WasmStore -> CString -> IO ()

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

-- | runWat runs a WebAssembly Text format file using C bindings
runWat :: String -> IO ()
runWat filename =
  withEngine $ \engine ->
    withStore engine $ \store ->
      do
        withCString filename $ \cStr -> runWatFile engine store cStr
