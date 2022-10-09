module Wasmtime.Engine (Engine (..), newEngine, touchEngine, EngineRef) where

import Foreign.ForeignPtr
import Foreign.Ptr

type EngineRef = ()

foreign import ccall "wasm.h wasm_engine_new" wasm_engine_new :: IO (Ptr EngineRef)

foreign import ccall "wasm.h &wasm_engine_delete" wasm_engine_delete :: FinalizerPtr EngineRef

newtype Engine = Engine (ForeignPtr EngineRef)
  deriving (Eq, Ord, Show)

newEngine :: IO Engine
newEngine = do
  e <- wasm_engine_new
  fp <- newForeignPtr wasm_engine_delete e
  return $ Engine fp

touchEngine :: Engine -> IO ()
touchEngine (Engine e) = touchForeignPtr e
