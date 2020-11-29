module Builder.Runtime (Runtime (..), WasmEngine, WasmStore, WasmLinker, withRuntime) where

import Foreign.Ptr

type WasmEngine = Ptr ()

type WasmStore = Ptr ()

type WasmLinker = Ptr ()

data Runtime = Runtime
  { engine :: WasmEngine,
    store :: WasmStore,
    linker :: WasmLinker
  }
  deriving (Show)

foreign import ccall "runtime.h create_engine" createEngine :: IO WasmEngine

foreign import ccall "runtime.h delete_engine" deleteEngine :: WasmEngine -> IO ()

foreign import ccall "runtime.h create_store" createStore :: WasmEngine -> IO WasmStore

foreign import ccall "runtime.h delete_store" deleteStore :: WasmStore -> IO ()

foreign import ccall "runtime.h create_linker" createLinker :: WasmStore -> IO WasmLinker

foreign import ccall "runtime.h delete_linker" deleteLinker :: WasmLinker -> IO ()

-- | withRuntime executes a callback with a Runtime
withRuntime :: (Runtime -> IO a) -> IO a
withRuntime cb =
  withEngine $ \engine ->
    withStore engine $ \store ->
      withLinker store $ \linker ->
        cb $ Runtime {engine, store, linker}

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

-- | withLinker runs a callback with a WASM Linker
withLinker :: WasmStore -> (WasmLinker -> IO a) -> IO a
withLinker store cb =
  do
    wasmLinker <- createLinker store
    result <- cb wasmLinker
    deleteLinker wasmLinker
    return result
