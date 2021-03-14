module Builder.Instance (Instance, callFunc, callFuncErr, returningByteString, withInstance, withWATString) where

import Builder.CTypes
import Builder.Runtime
import Builder.WASI
import qualified Codec.Binary.UTF8.String as UTF8
import Data.ByteString (ByteString, empty, pack)
import Data.ByteString.UTF8 (toString)
import Data.Maybe
import Foreign.C.String
import Foreign.C.Types (CChar (..), CInt (..), CSize (..))
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Marshal.Array (peekArray, withArrayLen)
import Foreign.Ptr
import Foreign.Storable (peek)

data Instance = Instance
  { wasmModule :: WasmModule,
    wasmInstance :: WasmInstance
  }
  deriving (Show)

foreign import ccall "runtime.h create_module" createModule :: WasmEngine -> CSize -> Ptr CChar -> IO WasmModule

foreign import ccall "runtime.h delete_module" deleteModule :: WasmModule -> IO ()

foreign import ccall "runtime.h create_instance" createInstance :: WasmLinker -> WasmModule -> IO WasmInstance

foreign import ccall "runtime.h delete_instance" deleteInstance :: WasmInstance -> IO ()

foreign import ccall "runtime.h call_func" callFuncC :: WasmInstance -> CInt -> IO ()

foreign import ccall "runtime.h call_func_with_error" callFuncWithErrorC :: WasmInstance -> CInt -> Ptr CSize -> IO (Ptr CChar)

foreign import ccall "runtime.h find_func_index" findFuncIndexC :: WasmModule -> CString -> IO CInt

-- | withInstance executes a callback with access to an Instance
withInstance :: Runtime -> String -> String -> (Instance -> IO a) -> IO a
withInstance runtime stderrPath watStr cb =
  withWasmModule (engine runtime) watStr $ \wasmModule ->
    do
      linkWasi runtime stderrPath
      withWasmInstance (linker runtime) wasmModule $ \wasmInstance ->
        cb $ Instance {wasmModule, wasmInstance}

linkWasi :: Runtime -> String -> IO ()
linkWasi runtime rawStderrPath =
  withCString "wasi_unstable" $ \wasiModuleName ->
    withCString rawStderrPath $ \stderrPath ->
      do
        config <- wasiConfigNew
        wasiConfigInheritArgv config
        wasiConfigInheritEnv config
        wasiConfigInheritStdin config
        wasiConfigInheritStdout config
        if null rawStderrPath
          then wasiConfigInheritStderr config
          else wasiConfigSetStderrFile config stderrPath

        wasi <- wasiInstanceNew (store runtime) wasiModuleName config nullPtr
        _err <- wasmtimeLinkerDefineWasi (linker runtime) wasi

        return ()

-- | callFunc calls a function by its exported name on an Instance
callFunc :: Instance -> String -> IO ()
callFunc inst funcExport =
  do
    idx <- findFuncIndex (wasmModule inst) funcExport
    callFuncC (wasmInstance inst) idx

-- | callFuncErr calls a function by its exported name expecting an error
callFuncErr :: Instance -> String -> IO (Maybe String)
callFuncErr inst funcExport =
  do
    byteString <- returningMaybeByteString $ \errSizePtr ->
      do
        idx <- findFuncIndex (wasmModule inst) funcExport
        callFuncWithErrorC (wasmInstance inst) idx errSizePtr
    return $ fmap toString byteString

-- | withWATString loads a WebAssembly Text Format string into a C consumable char array
withWATString :: String -> (CSize -> Ptr CChar -> IO a) -> IO a
withWATString watStr cb = withArrayLen (convertStringToCChars watStr) $ \watSize watPtr -> cb (fromIntegral watSize) watPtr

-- | returningByteString invokes a callback and converts the result to a ByteString
returningByteString :: (Ptr CSize -> IO (Ptr CChar)) -> IO ByteString
returningByteString cb =
  do
    byteString <- returningMaybeByteString cb
    return $ fromMaybe empty byteString

-- | returningMaybeByteString invokes a callback and converts the result to a Maybe ByteString
returningMaybeByteString :: (Ptr CSize -> IO (Ptr CChar)) -> IO (Maybe ByteString)
returningMaybeByteString cb =
  alloca $ \returningSizePtr ->
    do
      returningPtr <- cb returningSizePtr
      if returningPtr == nullPtr
        then return Nothing
        else do
          returningSize <- peek returningSizePtr
          returningBin <- peekArray (fromIntegral returningSize) returningPtr
          free returningPtr
          return $ Just $ convertCCharsToByteString returningBin

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
convertStringToCChars = map fromIntegral . UTF8.encode

-- | convertCCharsToByteString converts a CChar array to a ByteString
convertCCharsToByteString :: [CChar] -> ByteString
convertCCharsToByteString = pack . map fromIntegral
