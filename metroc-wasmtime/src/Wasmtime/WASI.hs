module Wasmtime.WASI where

import Foreign.Ptr

type WasiConfigRef = ()

foreign import ccall "wasi.h wasi_config_new" wasi_config_new :: IO (Ptr WasiConfigRef)

foreign import ccall "wasi.h wasi_config_inherit_argv" wasi_config_inherit_argv :: Ptr WasiConfigRef -> IO ()

foreign import ccall "wasi.h wasi_config_inherit_env" wasi_config_inherit_env :: Ptr WasiConfigRef -> IO ()

foreign import ccall "wasi.h wasi_config_inherit_stdin" wasi_config_inherit_stdin :: Ptr WasiConfigRef -> IO ()

foreign import ccall "wasi.h wasi_config_inherit_stdout" wasi_config_inherit_stdout :: Ptr WasiConfigRef -> IO ()

foreign import ccall "wasi.h wasi_config_inherit_stderr" wasi_config_inherit_stderr :: Ptr WasiConfigRef -> IO ()

foreign import ccall "wasi.h wasi_config_delete" wasi_config_delete :: Ptr WasiConfigRef -> IO ()

newtype WasiConfig = WasiConfig (Ptr WasiConfigRef)
  deriving (Eq, Ord, Show)

newWasiConfig :: IO WasiConfig
newWasiConfig = WasiConfig <$> wasi_config_new

wasiConfigInheritArgv :: WasiConfig -> IO ()
wasiConfigInheritArgv (WasiConfig p) = wasi_config_inherit_argv p

wasiConfigInheritEnv :: WasiConfig -> IO ()
wasiConfigInheritEnv (WasiConfig p) = wasi_config_inherit_env p

wasiConfigInheritStdin :: WasiConfig -> IO ()
wasiConfigInheritStdin (WasiConfig p) = wasi_config_inherit_stdin p

wasiConfigInheritStdout :: WasiConfig -> IO ()
wasiConfigInheritStdout (WasiConfig p) = wasi_config_inherit_stdout p

wasiConfigInheritStderr :: WasiConfig -> IO ()
wasiConfigInheritStderr (WasiConfig p) = wasi_config_inherit_stderr p

deleteWasiConfig :: WasiConfig -> IO ()
deleteWasiConfig (WasiConfig cfg) = wasi_config_delete cfg
