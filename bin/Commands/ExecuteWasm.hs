{-# LANGUAGE ForeignFunctionInterface #-}

module Commands.ExecuteWasm (runWat) where

import Foreign.C.String
import Foreign.Ptr

type WasmEngine = Ptr ()

foreign import ccall "runtime.h create_engine" createEngine :: IO WasmEngine

foreign import ccall "runtime.h delete_engine" deleteEngine :: WasmEngine -> IO ()

foreign import ccall "runtime.h run_wat_file" runWatFile :: WasmEngine -> CString -> IO ()

-- | runWat runs a WebAssembly Text format file using C bindings
runWat :: String -> IO ()
runWat filename =
  do
    engine <- createEngine
    withCString filename $ \cStr -> runWatFile engine cStr
    deleteEngine engine
