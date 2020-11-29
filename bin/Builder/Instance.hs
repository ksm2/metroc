module Builder.Instance (Instance, callFunc, returningByteString, withInstance, withWATString) where

import Builder.Runtime
import qualified Codec.Binary.UTF8.String as UTF8
import Data.ByteString (ByteString, pack)
import Foreign.C.String
import Foreign.C.Types (CChar (..), CInt (..), CSize (..))
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Marshal.Array (peekArray, withArrayLen)
import Foreign.Ptr
import Foreign.Storable (peek)

type WasmModule = Ptr ()

type WasmInstance = Ptr ()

data Instance = Instance
  { wasmModule :: WasmModule,
    wasmInstance :: WasmInstance
  }
  deriving (Show)

foreign import ccall "runtime.h create_module" createModule :: WasmEngine -> CSize -> Ptr CChar -> IO WasmModule

foreign import ccall "runtime.h delete_module" deleteModule :: WasmModule -> IO ()

foreign import ccall "runtime.h create_instance" createInstance :: WasmLinker -> WasmModule -> IO WasmInstance

foreign import ccall "runtime.h delete_instance" deleteInstance :: WasmInstance -> IO ()

foreign import ccall "runtime.h link_wasi" linkWasi :: WasmStore -> WasmLinker -> IO ()

foreign import ccall "runtime.h call_func" callFuncC :: WasmInstance -> CInt -> IO ()

foreign import ccall "runtime.h find_func_index" findFuncIndexC :: WasmModule -> CString -> IO CInt

-- | withInstance executes a callback with access to an Instance
withInstance :: Runtime -> String -> (Instance -> IO a) -> IO a
withInstance runtime watStr cb =
  withWasmModule (engine runtime) watStr $ \wasmModule ->
    do
      linkWasi (store runtime) (linker runtime)
      withWasmInstance (linker runtime) wasmModule $ \wasmInstance ->
        cb $ Instance {wasmModule, wasmInstance}

-- | callFunc calls a function by its exported name on an Instance
callFunc :: Instance -> String -> IO ()
callFunc inst funcExport =
  do
    idx <- findFuncIndex (wasmModule inst) funcExport
    callFuncC (wasmInstance inst) idx

-- | withWATString loads a WebAssembly Text Format string into a C consumable char array
withWATString :: String -> (CSize -> Ptr CChar -> IO a) -> IO a
withWATString watStr cb = withArrayLen (convertStringToCChars watStr) $ \watSize watPtr -> cb (fromIntegral watSize) watPtr

-- | returningByteString invokes a callback and converts the result to a ByteString
returningByteString :: (Ptr CSize -> IO (Ptr CChar)) -> IO ByteString
returningByteString cb =
  alloca $ \returningSizePtr ->
    do
      returningPtr <- cb returningSizePtr
      returningSize <- peek returningSizePtr
      returningBin <- peekArray (fromIntegral returningSize) returningPtr
      free returningPtr
      return $ convertCCharsToByteString returningBin

-- | withWasmModule runs a callback with a WASM Module
withWasmModule :: WasmEngine -> String -> (WasmModule -> IO a) -> IO a
withWasmModule engine watStr cb =
  withWATString watStr $ \watSize watPtr ->
    do
      wasmModule <- createModule engine watSize watPtr
      result <- cb wasmModule
      deleteModule wasmModule
      return result

-- | withWasmInstance runs a callback with a WASM Instance
withWasmInstance :: WasmLinker -> WasmModule -> (WasmInstance -> IO a) -> IO a
withWasmInstance wasmLinker wasmModule cb =
  do
    wasmInstance <- createInstance wasmLinker wasmModule
    result <- cb wasmInstance
    deleteInstance wasmInstance
    return result

-- | findFuncIndex finds a func export inside a module
findFuncIndex :: WasmModule -> String -> IO CInt
findFuncIndex m s = withCString s $ \cStr -> findFuncIndexC m cStr

-- | convertStringToCChars converts a String to a CChar array
convertStringToCChars :: String -> [CChar]
convertStringToCChars = (map fromIntegral) . UTF8.encode

-- | convertCCharsToByteString converts a CChar array to a ByteString
convertCCharsToByteString :: [CChar] -> ByteString
convertCCharsToByteString = pack . (map fromIntegral)
