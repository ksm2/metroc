module Wasmtime.Linker (Linker (..), newLinker, linkerConfigureWasi, linkerModule, linkerGetDefault, linkerGet) where

import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc (malloc)
import Foreign.Ptr
import Wasmtime.Engine
import Wasmtime.Func
import Wasmtime.Module
import Wasmtime.Store
import Wasmtime.WASI

type LinkerRef = ()

foreign import ccall "wasmtime.h wasmtime_linker_new" wasmtime_linker_new :: Ptr EngineRef -> IO (Ptr LinkerRef)

foreign import ccall "wasmtime.h wasmtime_linker_define_wasi" wasmtime_linker_define_wasi :: Ptr LinkerRef -> IO (Ptr e)

foreign import ccall "wasmtime.h wasmtime_context_set_wasi" wasmtime_context_set_wasi :: Ptr ContextRef -> Ptr WasiConfigRef -> IO (Ptr e)

foreign import ccall "wasmtime.h wasmtime_linker_module" wasmtime_linker_module :: Ptr LinkerRef -> Ptr ContextRef -> CString -> Int -> Ptr ModuleRef -> IO (Ptr e)

foreign import ccall "wasmtime.h wasmtime_linker_get_default" wasmtime_linker_get_default :: Ptr LinkerRef -> Ptr ContextRef -> CString -> Int -> Ptr FuncStruct -> IO (Ptr e)

foreign import ccall "wasmtime.h wasmtime_linker_get" wasmtime_linker_get :: Ptr LinkerRef -> Ptr ContextRef -> CString -> Int -> CString -> Int -> Ptr FuncStruct -> IO Bool

foreign import ccall "wasmtime.h &wasmtime_linker_delete" wasmtime_linker_delete :: FinalizerPtr LinkerRef

data Linker = Linker
  { linkerRef :: !(ForeignPtr LinkerRef),
    linkerStore :: !Store
  }
  deriving (Eq, Ord, Show)

newLinker :: Engine -> IO Linker
newLinker e@(Engine engine) = do
  store <- newStore e
  withForeignPtr engine $ \enginePtr -> do
    linkerRef <- wasmtime_linker_new enginePtr
    fp <- newForeignPtr wasmtime_linker_delete linkerRef
    return $ Linker fp store

linkerConfigureWasi :: Linker -> WasiConfig -> IO ()
linkerConfigureWasi linker (WasiConfig config) =
  withLinker linker $ \l s -> do
    let c = storeContext s
    _ <- wasmtime_linker_define_wasi l
    _ <- wasmtime_context_set_wasi c config
    return ()

linkerModule :: Linker -> String -> Module -> IO ()
linkerModule linker name (Module wasmModule) =
  withLinker linker $ \l store ->
    withForeignPtr wasmModule $ \m ->
      withCStringLen name $ \(namePtr, nameLen) -> do
        let context = storeContext store
        _ <- wasmtime_linker_module l context namePtr nameLen m
        return ()

linkerGetDefault :: Linker -> String -> IO Func
linkerGetDefault linker moduleName =
  withLinker linker $ \l context ->
    withCStringLen moduleName $ \(moduleNamePtr, moduleNameLen) -> do
      func <- malloc
      _ <- wasmtime_linker_get_default l (storeContext context) moduleNamePtr moduleNameLen func
      touchStore context
      return $ Func func context

linkerGet :: Linker -> String -> String -> IO Func
linkerGet linker moduleName exportName =
  withLinker linker $ \l context ->
    withCStringLen moduleName $ \(moduleNamePtr, moduleNameLen) ->
      withCStringLen exportName $ \(exportNamePtr, exportNameLen) -> do
        func <- malloc
        _ <- wasmtime_linker_get l (storeContext context) moduleNamePtr moduleNameLen exportNamePtr exportNameLen func
        touchStore context
        return $ Func (plusPtr func 8) context

withLinker :: Linker -> (Ptr LinkerRef -> Store -> IO a) -> IO a
withLinker l cb = withForeignPtr (linkerRef l) $ \lr -> cb lr (linkerStore l)
