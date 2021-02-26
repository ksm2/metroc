module MetroLang.Lang.Lexer (lexer) where

import Data.Char
import MetroLang.Lang.Exception
import MetroLang.Lang.Keywords
import MetroLang.Lang.Token

lexer :: (Token -> P a) -> P a
lexer cont [] = cont TokenEOF []
lexer cont ('/' : '/' : cs) = lexSingleLineComment cont cs
lexer cont ('/' : '*' : cs) = lexMultiLineComment cont cs
lexer cont ('\n' : cs) = \col line -> cont TokenEOS cs 1 (line + 1)
lexer cont (c : cs)
  | isSpace c = \col -> lexer cont cs (col + 1)
  | isAlpha c = lexVar cont (c : cs)
  | isDigit c = lexNum cont (c : cs)
lexer cont ('=' : cs) = \col -> cont TokenEq cs (col + 1)
lexer cont ('+' : cs) = \col -> cont TokenPlus cs (col + 1)
lexer cont ('-' : cs) = \col -> cont TokenMinus cs (col + 1)
lexer cont ('*' : cs) = \col -> cont TokenTimes cs (col + 1)
lexer cont ('/' : cs) = \col -> cont TokenDiv cs (col + 1)
lexer cont ('(' : cs) = \col -> cont TokenLParen cs (col + 1)
lexer cont (')' : cs) = \col -> cont TokenRParen cs (col + 1)
lexer cont ('{' : cs) = \col -> cont TokenLBrace cs (col + 1)
lexer cont ('}' : cs) = \col -> cont TokenRBrace cs (col + 1)
lexer cont ('[' : cs) = \col -> cont TokenLBrack cs (col + 1)
lexer cont (']' : cs) = \col -> cont TokenRBrack cs (col + 1)
lexer cont ('<' : cs) = \col -> cont TokenLT cs (col + 1)
lexer cont ('>' : cs) = \col -> cont TokenGT cs (col + 1)
lexer cont ('.' : cs) = \col -> cont TokenDot cs (col + 1)
lexer cont (',' : cs) = \col -> cont TokenComma cs (col + 1)
lexer cont (ch : cs) = failP ("Unknown char: " ++ [ch]) ""

lexSingleLineComment :: (Token -> P a) -> P a
lexSingleLineComment cont ('\n' : cs) = \col line -> cont TokenEOS cs 1 (line + 1)
lexSingleLineComment cont (_ : cs) = lexSingleLineComment cont cs
lexSingleLineComment cont [] = cont TokenEOF []

lexMultiLineComment :: (Token -> P a) -> P a
lexMultiLineComment cont ('*' : '/' : cs) = \col -> lexer cont cs (col + 2)
lexMultiLineComment cont ('\n' : cs) = \col line -> lexMultiLineComment cont cs 1 (line + 1)
lexMultiLineComment cont (_ : cs) = \col -> lexMultiLineComment cont cs (col + 1)
lexMultiLineComment cont [] = cont TokenEOF []

lexNum :: (Token -> P a) -> P a
lexNum cont cs = \col -> cont (TokenInt (read num)) rest (col + (length num))
  where
    (num, rest) = span isDigit cs

lexVar :: (Token -> P a) -> P a
lexVar cont cs =
  case findKeywordToken keyword of
    Just (_, token) -> \col -> cont token rest (col + (length keyword))
    Nothing -> \col -> cont (TokenIdentifier keyword) rest (col + (length keyword))
  where
    (keyword, rest) = span isAlpha cs
