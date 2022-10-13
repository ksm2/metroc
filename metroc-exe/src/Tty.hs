{-# LANGUAGE CPP #-}

module Tty where

import Control.Monad
#if !defined(mingw32_HOST_OS) || defined(__MINGW32__)
import System.Posix.IO (stdOutput)
import System.Posix.Terminal (queryTerminal)
#endif

-- | isTty returns whether stdout is a TTY
isTty :: IO Bool
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
isTty = return False
#else
isTty = queryTerminal stdOutput
#endif

-- | withTty transforms the given text using a callback if stdout is a TTY
withTty :: a -> a -> IO a
withTty nonTtyText ttyText =
  do
    tty <- isTty
    return $ if tty then ttyText else nonTtyText

-- | putTty puts the first argument's text if not a TTY, otherwise the second argument
putTty :: String -> String -> IO ()
putTty nonTtyText ttyText =
  do
    line <- withTty nonTtyText ttyText
    putStr line

-- | putTtyLn puts the first argument's text with a new line if not a TTY, otherwise the second argument
putTtyLn :: String -> String -> IO ()
putTtyLn nonTtyText ttyText =
  do
    line <- withTty nonTtyText ttyText
    putStrLn line

-- | ifTty executes its argument only if stdout is a TTY
ifTty :: IO () -> IO ()
ifTty cb =
  do
    tty <- isTty
    when tty cb
