module Wasmtime.Module where

import Data.Word (Word8)
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Wasmtime.ByteVec
import Wasmtime.Engine

type ModuleRef = ()

foreign import ccall "wasmtime.h wasmtime_module_new" wasmtime_module_new :: Ptr EngineRef -> Ptr Word8 -> CSize -> Ptr (Ptr ModuleRef) -> IO (Ptr e)

foreign import ccall "wasmtime.h &wasmtime_module_delete" wasmtime_module_delete :: FinalizerPtr ModuleRef

newtype Module = Module (ForeignPtr ModuleRef)
  deriving (Eq, Ord, Show)

newModule :: Engine -> ByteVec -> IO Module
newModule (Engine engine) (ByteVec wasm) =
  withForeignPtr engine $ \e ->
    withForeignPtr wasm $ \bv -> do
      modRef <- malloc
      bvs <- peek bv
      err <- wasmtime_module_new e (byteVecData bvs) (byteVecSize bvs) modRef
      ref <- peek modRef
      fp <- newForeignPtr wasmtime_module_delete ref
      return $ Module fp
