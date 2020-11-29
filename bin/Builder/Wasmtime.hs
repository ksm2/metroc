{-# LANGUAGE ForeignFunctionInterface #-}

module Builder.Wasmtime (runWAT, watToWasm) where

import Builder.Instance (callFunc, returningByteString, withInstance, withWATString)
import Builder.Runtime (withRuntime)
import Data.ByteString (ByteString)
import Foreign.C.Types (CChar (..), CSize (..))
import Foreign.Ptr (Ptr)

foreign import ccall "runtime.h wat_to_wasm" watToWasmC :: CSize -> Ptr CChar -> Ptr CSize -> IO (Ptr CChar)

-- | runWAT runs a WebAssembly Text format string using C bindings
runWAT :: String -> IO ()
runWAT watStr =
  withRuntime $ \runtime ->
    withInstance runtime watStr $ \wasmInstance ->
      callFunc wasmInstance "main"

-- | watToWasm compiles WebAssembly Text Format to Binary Format using C bindings
watToWasm :: String -> IO ByteString
watToWasm watStr =
  returningByteString $ \wasmSizePtr ->
    withWATString watStr $ \watSize watPtr ->
      watToWasmC watSize watPtr wasmSizePtr
