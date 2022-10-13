module MetroLang.Lang.Parlex where

import Control.Applicative as App (Applicative (..))
import Data.Word (Word8)
import MetroLang.Lang.Error
import MetroLang.Location

type Byte = Word8

data ParlexState = ParlexState
  { alex_pos :: !Position, -- position at current input location
    alex_inp :: String, -- the current input
    alex_chr :: !Char, -- the character before the input
    alex_bytes :: [Byte], -- rest of the bytes for the current char
    alex_scd :: !Int, -- the current startcode
    alex_source :: Source,
    alex_string :: String
  }

newtype Parlex a = Parlex {unParser :: ParlexState -> Either MetroError (ParlexState, a)}

instance Functor Parlex where
  fmap f a = Parlex $ \s -> case unParser a s of
    Left msg -> Left msg
    Right (s', a') -> Right (s', f a')

instance Applicative Parlex where
  pure a = Parlex $ \s -> Right (s, a)
  fa <*> a = Parlex $ \s -> case unParser fa s of
    Left msg -> Left msg
    Right (s', f) -> case unParser a s' of
      Left msg -> Left msg
      Right (s'', b) -> Right (s'', f b)

instance Monad Parlex where
  m >>= k = Parlex $ \s -> case unParser m s of
    Left msg -> Left msg
    Right (s', a) -> unParser (k a) s'
  return = App.pure
