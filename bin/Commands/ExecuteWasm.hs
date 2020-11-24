{-# LANGUAGE ForeignFunctionInterface #-}

module Commands.ExecuteWasm (runWat) where

import Foreign.C.String

foreign import ccall "runtime.h executeWasm" executeWasm :: CString -> IO ()

-- | runWat runs a WebAssembly Text format file using C bindings
runWat :: String -> IO ()
runWat filename = withCString filename $ \cStr -> executeWasm cStr
