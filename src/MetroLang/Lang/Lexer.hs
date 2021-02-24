module MetroLang.Lang.Lexer (lexer) where

import Data.Char
import MetroLang.Lang.Exception
import MetroLang.Lang.Token

lexer :: (Token -> P a) -> P a
lexer cont [] = cont TokenEOF []
lexer cont ('\n' : cs) = \col line -> lexer cont cs 0 (line + 1)
lexer cont (c : cs)
  | isSpace c = \col -> lexer cont cs (col + 1)
  | isAlpha c = lexVar cont (c : cs)
  | isDigit c = lexNum cont (c : cs)
lexer cont ('=' : cs) = \col -> cont TokenEq cs (col + 1)
lexer cont ('+' : cs) = \col -> cont TokenPlus cs (col + 1)
lexer cont ('-' : cs) = \col -> cont TokenMinus cs (col + 1)
lexer cont ('*' : cs) = \col -> cont TokenTimes cs (col + 1)
lexer cont ('/' : cs) = \col -> cont TokenDiv cs (col + 1)
lexer cont ('(' : cs) = \col -> cont TokenOB cs (col + 1)
lexer cont (')' : cs) = \col -> cont TokenCB cs (col + 1)

lexNum :: (Token -> P a) -> P a
lexNum cont cs = \col -> cont (TokenInt (read num)) rest (col + (length num))
  where
    (num, rest) = span isDigit cs

lexVar :: (Token -> P a) -> P a
lexVar cont cs =
  case span isAlpha cs of
    ("let", rest) -> \col -> cont TokenLet rest (col + 3)
    ("in", rest) -> \col -> cont TokenIn rest (col + 2)
    (var, rest) -> \col -> cont (TokenVar var) rest (col + (length var))
