module Wasmtime.Func where

import Foreign
import Foreign.C
import Wasmtime.Store

#include "wasmtime.h"

data Func = Func {
    funcPtr :: !(Ptr FuncStruct)
  , funcStore :: !Store
} deriving (Eq, Ord, Show)

data FuncStruct = FuncStruct {
    storeId :: Word64
  , index :: CSize
} deriving (Eq, Ord, Show)

instance Storable FuncStruct where
  sizeOf _ = #{size wasmtime_func_t}
  alignment _ = #{alignment wasmtime_func_t}

  peek p =
    FuncStruct
      <$> #{peek wasmtime_func_t, store_id} p
      <*> #{peek wasmtime_func_t, index} p

  poke p func = do
      #{poke wasmtime_func_t, store_id} p $ storeId func
      #{poke wasmtime_func_t, index} p $ index func

foreign import ccall "wasmtime.h wasmtime_func_call" wasmtime_func_call :: Ptr ContextRef -> Ptr FuncStruct -> Ptr a -> CSize -> Ptr a -> CSize -> Ptr t -> IO (Ptr e)

funcCall :: Func -> IO (Maybe String)
funcCall func = do
  let store = funcStore func
  err <- wasmtime_func_call (storeContext store) (funcPtr func) nullPtr 0 nullPtr 0 nullPtr
  touchStore store
  return $ if err == nullPtr then Nothing else Just "Error"
