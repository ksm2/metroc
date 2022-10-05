module Wasmtime.ByteVec (ByteVec (..), ByteVecStruct (..), byteVecToByteString, newByteVec) where

import qualified Data.ByteString as B
import Data.Word (Word8)
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr
import Foreign.Storable

#include "wasmtime.h"

data ByteVec = ByteVec !(ForeignPtr ByteVecStruct)
  deriving (Eq, Ord, Show)

data ByteVecStruct = ByteVecStruct {
    byteVecSize :: CSize
  , byteVecData :: Ptr Word8
} deriving (Eq, Ord, Show)

foreign import ccall "wasm.h &wasm_byte_vec_delete" wasm_byte_vec_delete :: FinalizerPtr ByteVecStruct

instance Storable ByteVecStruct where
  sizeOf _ = #{size wasm_byte_vec_t}
  alignment _ = #{alignment wasm_byte_vec_t}

  peek ptr =
    ByteVecStruct
      <$> #{peek wasm_byte_vec_t, size} ptr
      <*> #{peek wasm_byte_vec_t, data} ptr

  poke ptr bvs = do
    #{poke wasm_byte_vec_t, size} ptr $ byteVecSize bvs
    #{poke wasm_byte_vec_t, data} ptr $ byteVecData bvs

-- | byteVecToByteString converts a wasm_byte_vec_t to a ByteString
byteVecToByteString :: ByteVec -> IO B.ByteString
byteVecToByteString (ByteVec ptr) = do
  withForeignPtr ptr $ \bv -> do
    bvs <- peek bv
    arr <- peekArray (fromIntegral (byteVecSize bvs)) (byteVecData bvs)
    return $ B.pack arr

newByteVec :: Ptr ByteVecStruct -> IO ByteVec
newByteVec ptr = do
  fp <- newForeignPtr wasm_byte_vec_delete ptr
  return $ ByteVec fp
