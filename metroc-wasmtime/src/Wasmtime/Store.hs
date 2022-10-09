module Wasmtime.Store (Store (..), ContextRef, newStore, touchStore, StoreRef) where

import Foreign.ForeignPtr
import Foreign.Ptr
import Wasmtime.Engine

type StoreRef = ()

type ContextRef = ()

foreign import ccall "wasmtime.h wasmtime_store_new" wasmtime_store_new :: Ptr EngineRef -> Ptr a -> Ptr b -> IO (Ptr StoreRef)

foreign import ccall "wasmtime.h wasmtime_store_context" wasmtime_store_context :: Ptr StoreRef -> IO (Ptr ContextRef)

foreign import ccall "wasmtime.h &wasmtime_store_delete" wasmtime_store_delete :: FinalizerPtr StoreRef

data Store = Store
  { storePtr :: !(ForeignPtr StoreRef),
    storeContext :: !(Ptr ContextRef)
  }
  deriving (Eq, Ord, Show)

newStore :: Engine -> IO Store
newStore (Engine engine) =
  withForeignPtr engine $ \e -> do
    store <- wasmtime_store_new e nullPtr nullPtr
    ctx <- wasmtime_store_context store
    fp <- newForeignPtr wasmtime_store_delete store
    return $ Store fp ctx

touchStore :: Store -> IO ()
touchStore = touchForeignPtr . storePtr
