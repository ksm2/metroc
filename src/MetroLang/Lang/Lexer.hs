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
lexer cont s@(c : cs)
  | isSpace c = \col -> lexer cont cs (col + 1)
  | isAlpha c = lexVar cont s
  | isDigit c = lexNum cont s
  | c == '"' = lexStr cont s
lexer cont ('!' : '=' : cs) = \col -> cont TokenExclEq cs (col + 2)
lexer cont ('%' : '=' : cs) = \col -> cont TokenRemEq cs (col + 2)
lexer cont ('%' : '>' : '=' : cs) = \col -> cont TokenRemGtEq cs (col + 3)
lexer cont ('%' : '>' : cs) = \col -> cont TokenRemGt cs (col + 2)
lexer cont ('%' : cs) = \col -> cont TokenRem cs (col + 1)
lexer cont ('&' : '=' : cs) = \col -> cont TokenAmpEq cs (col + 2)
lexer cont ('&' : cs) = \col -> cont TokenAmp cs (col + 1)
lexer cont ('(' : cs) = \col -> cont TokenLParen cs (col + 1)
lexer cont (')' : cs) = \col -> cont TokenRParen cs (col + 1)
lexer cont ('*' : '=' : cs) = \col -> cont TokenMulEq cs (col + 2)
lexer cont ('*' : cs) = \col -> cont TokenMul cs (col + 1)
lexer cont ('+' : '=' : cs) = \col -> cont TokenPlusEq cs (col + 2)
lexer cont ('+' : cs) = \col -> cont TokenPlus cs (col + 1)
lexer cont (',' : cs) = \col -> cont TokenComma cs (col + 1)
lexer cont ('-' : '=' : cs) = \col -> cont TokenMinusEq cs (col + 2)
lexer cont ('-' : cs) = \col -> cont TokenMinus cs (col + 1)
lexer cont ('.' : cs) = \col -> cont TokenDot cs (col + 1)
lexer cont ('/' : '=' : cs) = \col -> cont TokenDivEq cs (col + 2)
lexer cont ('/' : cs) = \col -> cont TokenDiv cs (col + 1)
lexer cont (':' : '=' : cs) = \col -> cont TokenColonEq cs (col + 2)
lexer cont (':' : cs) = \col -> cont TokenColon cs (col + 2)
lexer cont ('<' : '%' : '=' : cs) = \col -> cont TokenLtRemEq cs (col + 3)
lexer cont ('<' : '%' : cs) = \col -> cont TokenLtRem cs (col + 2)
lexer cont ('<' : '<' : '=' : cs) = \col -> cont TokenLtLtEq cs (col + 3)
lexer cont ('<' : '<' : cs) = \col -> cont TokenLtLt cs (col + 2)
lexer cont ('<' : '=' : cs) = \col -> cont TokenLtEq cs (col + 2)
lexer cont ('<' : cs) = \col -> cont TokenLt cs (col + 1)
lexer cont ('=' : '=' : cs) = \col -> cont TokenEqEq cs (col + 2)
lexer cont ('=' : cs) = \col -> cont TokenEq cs (col + 1)
lexer cont ('>' : '=' : cs) = \col -> cont TokenGtEq cs (col + 2)
lexer cont ('>' : '>' : '=' : cs) = \col -> cont TokenGtGtEq cs (col + 3)
lexer cont ('>' : '>' : cs) = \col -> cont TokenGtGt cs (col + 2)
lexer cont ('>' : cs) = \col -> cont TokenGt cs (col + 1)
lexer cont ('?' : '.' : cs) = \col -> cont TokenQDot cs (col + 2)
lexer cont ('?' : cs) = \col -> cont TokenQ cs (col + 1)
lexer cont ('[' : cs) = \col -> cont TokenLBrack cs (col + 1)
lexer cont (']' : cs) = \col -> cont TokenRBrack cs (col + 1)
lexer cont ('^' : '=' : cs) = \col -> cont TokenCaretEq cs (col + 2)
lexer cont ('^' : cs) = \col -> cont TokenCaret cs (col + 1)
lexer cont ('{' : cs) = \col -> cont TokenLBrace cs (col + 1)
lexer cont ('|' : '=' : cs) = \col -> cont TokenBarEq cs (col + 2)
lexer cont ('|' : cs) = \col -> cont TokenBar cs (col + 1)
lexer cont ('}' : cs) = \col -> cont TokenRBrace cs (col + 1)
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

lexNumSuffix :: Int -> (Token -> P a) -> P a
lexNumSuffix num cont ('U' : cs) = \col -> cont (TokenInt num) cs (col + 1)
lexNumSuffix num cont cs = cont (TokenInt num) cs

lexNum :: (Token -> P a) -> P a
lexNum cont ('0' : 'x' : cs) =
  \col -> lexNumSuffix (parseHex num) cont rest (col + (length num + 2))
  where
    (num, rest) = span isHexDigit cs
lexNum cont cs =
  \col -> lexNumSuffix (read num) cont rest (col + (length num))
  where
    (num, rest) = span isDigit cs

parseHex "" = 0
parseHex [c] = parseHexChar c
parseHex cs = parseHexChar (last cs) + 16 * parseHex (init cs)

parseHexChar '0' = 0
parseHexChar '1' = 1
parseHexChar '2' = 2
parseHexChar '3' = 3
parseHexChar '4' = 4
parseHexChar '5' = 5
parseHexChar '6' = 6
parseHexChar '7' = 7
parseHexChar '8' = 8
parseHexChar '9' = 9
parseHexChar 'a' = 10
parseHexChar 'A' = 10
parseHexChar 'b' = 11
parseHexChar 'B' = 11
parseHexChar 'c' = 12
parseHexChar 'C' = 12
parseHexChar 'd' = 13
parseHexChar 'D' = 13
parseHexChar 'e' = 14
parseHexChar 'E' = 14
parseHexChar 'f' = 15
parseHexChar 'F' = 15

lexVar :: (Token -> P a) -> P a
lexVar cont cs =
  case findKeywordToken keyword of
    Just token -> \col -> cont token rest (col + (length keyword))
    Nothing -> \col -> cont (TokenIdentifier keyword) rest (col + (length keyword))
  where
    (keyword, rest) = span isIdentifierChar cs

lexStr :: (Token -> P a) -> P a
lexStr cont (c : cs) = \col -> lexStrRec "" cont cs (col + 1)

lexStrRec :: String -> (Token -> P a) -> P a
lexStrRec str cont ('\\' : '"' : cs) = \col -> lexStrRec ('"' : str) cont cs (col + 2)
lexStrRec str cont ('"' : cs) = \col -> cont (TokenString $ reverse str) cs (col + 1)
lexStrRec str cont (c : cs) = \col -> lexStrRec (c : str) cont cs (col + 1)
lexStrRec str cont [] = cont TokenEOF []

isIdentifierChar :: Char -> Bool
isIdentifierChar c = isAlphaNum c || c == '_'
