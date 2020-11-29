{-# LANGUAGE ForeignFunctionInterface #-}

module Builder.Conversion (watToWasm) where

import Builder.Instance (returningByteString, withWATString)
import Data.ByteString (ByteString)
import Foreign.C.Types (CChar (..), CSize (..))
import Foreign.Ptr (Ptr)

foreign import ccall "runtime.h wat_to_wasm" watToWasmC :: CSize -> Ptr CChar -> Ptr CSize -> IO (Ptr CChar)

-- | watToWasm compiles WebAssembly Text Format to Binary Format using C bindings
watToWasm :: String -> IO ByteString
watToWasm watStr =
  returningByteString $ \wasmSizePtr ->
    withWATString watStr $ \watSize watPtr ->
      watToWasmC watSize watPtr wasmSizePtr
