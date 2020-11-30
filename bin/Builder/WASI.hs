module Builder.WASI where

import Builder.CTypes
import Foreign.C.String
import Foreign.Ptr

type WasiConfig = Ptr ()

type WasiTrap = Ptr ()

foreign import ccall "wasi.h wasi_config_new" wasiConfigNew :: IO WasiConfig

foreign import ccall "wasi.h wasi_config_inherit_argv" wasiConfigInheritArgv :: WasiConfig -> IO ()

foreign import ccall "wasi.h wasi_config_inherit_env" wasiConfigInheritEnv :: WasiConfig -> IO ()

foreign import ccall "wasi.h wasi_config_inherit_stdin" wasiConfigInheritStdin :: WasiConfig -> IO ()

foreign import ccall "wasi.h wasi_config_inherit_stdout" wasiConfigInheritStdout :: WasiConfig -> IO ()

foreign import ccall "wasi.h wasi_config_set_stderr_file " wasiConfigSetStderrFile :: WasiConfig -> CString -> IO ()

foreign import ccall "wasi.h wasi_config_inherit_stderr" wasiConfigInheritStderr :: WasiConfig -> IO ()

foreign import ccall "wasi.h wasi_instance_new" wasiInstanceNew :: WasmStore -> CString -> WasiConfig -> WasiTrap -> IO WasmInstance

foreign import ccall "wasmtime.h wasmtime_linker_define_wasi" wasmtimeLinkerDefineWasi :: WasmLinker -> WasmInstance -> IO (Ptr ())
