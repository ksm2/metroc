module Wasmtime.WAT (wat2wasm) where

import Foreign.C.String
import Foreign.Marshal.Alloc (malloc)
import Foreign.Ptr
import Wasmtime.ByteVec

foreign import ccall "wasmtime.h wasmtime_wat2wasm" wasmtime_wat2wasm :: CString -> Int -> Ptr ByteVecStruct -> IO (Ptr e)

wat2wasm :: String -> IO ByteVec
wat2wasm watStr = do
  wasmPtr <- malloc
  withCStringLen watStr $ \(cStr, cSize) -> do
    _ <- wasmtime_wat2wasm cStr cSize wasmPtr
    newByteVec wasmPtr
