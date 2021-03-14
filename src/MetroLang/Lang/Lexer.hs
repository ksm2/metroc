module MetroLang.Lang.Lexer (lexer) where

import Data.Char
import MetroLang.Lang.Exception
import MetroLang.Lang.Keywords
import MetroLang.Lang.Token

lexer :: (Token -> P a) -> P a
lexer cont [] = cont TokenEOF []
lexer cont ('/' : '/' : cs) = lexSingleLineComment cont cs
lexer cont ('/' : '*' : cs) = lexMultiLineComment cont cs
lexer cont ('\n' : cs) = \col line -> lexNewLine cont cs 1 (line + 1)
lexer cont s@(c : cs)
  | isSpace c = \col -> lexer cont cs (col + 1)
  | isAlpha c = lexVar cont s
  | isDigit c = lexNum cont s
  | c == '"' = lexStr cont s
lexer cont s@('_' : c : cs)
  | isAlphaNum c = lexVar cont s
  | c == '_' = lexVar cont s
  | otherwise = \col -> cont TokenUnderscore cs (col + 1)
lexer cont ['_'] = \col -> cont TokenUnderscore [] (col + 1)
lexer cont ('{' : cs) = \col -> cont TokenLBrace cs (col + 1)
lexer cont ('}' : cs) = \col -> cont TokenRBrace cs (col + 1)
lexer cont ('(' : cs) = \col -> cont TokenLParen cs (col + 1)
lexer cont (')' : cs) = \col -> cont TokenRParen cs (col + 1)
lexer cont cs =
  case mToken of
    Just token -> \col -> cont token rest (col + consumed)
    Nothing -> failP ("Unknown char: " ++ [head rest]) ""
  where
    (mToken, rest, consumed) = lexOperator cs

lexNewLine :: (Token -> P a) -> P a
lexNewLine cont ('\n' : cs) = \col line -> lexNewLine cont cs 1 (line + 1)
lexNewLine cont (c : cs)
  | isSpace c = \col -> lexNewLine cont cs (col + 1)
lexNewLine cont cs =
  case mToken of
    Just token -> \col -> cont token rest (col + consumed)
    Nothing -> cont TokenEOS rest
  where
    (mToken, rest, consumed) = lexOperator cs

lexOperator :: String -> (Maybe Token, String, Int)
lexOperator = \case
  ('!' : '=' : cs) -> (Just TokenExclEq, cs, 2)
  ('%' : '=' : cs) -> (Just TokenRemEq, cs, 2)
  ('%' : '>' : '=' : cs) -> (Just TokenRemGtEq, cs, 3)
  ('%' : '>' : cs) -> (Just TokenRemGt, cs, 2)
  ('%' : cs) -> (Just TokenRem, cs, 1)
  ('&' : '=' : cs) -> (Just TokenAmpEq, cs, 2)
  ('&' : cs) -> (Just TokenAmp, cs, 1)
  ('*' : '=' : cs) -> (Just TokenMulEq, cs, 2)
  ('*' : cs) -> (Just TokenMul, cs, 1)
  ('+' : '=' : cs) -> (Just TokenPlusEq, cs, 2)
  ('+' : cs) -> (Just TokenPlus, cs, 1)
  (',' : cs) -> (Just TokenComma, cs, 1)
  ('-' : '=' : cs) -> (Just TokenMinusEq, cs, 2)
  ('-' : cs) -> (Just TokenMinus, cs, 1)
  ('.' : cs) -> (Just TokenDot, cs, 1)
  ('/' : '=' : cs) -> (Just TokenDivEq, cs, 2)
  cs@('/' : '*' : _) -> (Nothing, cs, 0)
  cs@('/' : '/' : _) -> (Nothing, cs, 0)
  ('/' : cs) -> (Just TokenDiv, cs, 1)
  (':' : '=' : cs) -> (Just TokenColonEq, cs, 2)
  (':' : cs) -> (Just TokenColon, cs, 2)
  ('<' : '%' : '=' : cs) -> (Just TokenLtRemEq, cs, 3)
  ('<' : '%' : cs) -> (Just TokenLtRem, cs, 2)
  ('<' : '<' : '=' : cs) -> (Just TokenLtLtEq, cs, 3)
  ('<' : '<' : cs) -> (Just TokenLtLt, cs, 2)
  ('<' : '=' : cs) -> (Just TokenLtEq, cs, 2)
  ('<' : cs) -> (Just TokenLt, cs, 1)
  ('=' : '=' : cs) -> (Just TokenEqEq, cs, 2)
  ('=' : '>' : cs) -> (Just TokenEqGt, cs, 2)
  ('=' : cs) -> (Just TokenEq, cs, 1)
  ('>' : '=' : cs) -> (Just TokenGtEq, cs, 2)
  ('>' : '>' : '=' : cs) -> (Just TokenGtGtEq, cs, 3)
  ('>' : '>' : cs) -> (Just TokenGtGt, cs, 2)
  ('>' : cs) -> (Just TokenGt, cs, 1)
  ('?' : '.' : cs) -> (Just TokenQDot, cs, 2)
  ('?' : cs) -> (Just TokenQ, cs, 1)
  ('[' : cs) -> (Just TokenLBrack, cs, 1)
  (']' : cs) -> (Just TokenRBrack, cs, 1)
  ('^' : '=' : cs) -> (Just TokenCaretEq, cs, 2)
  ('^' : cs) -> (Just TokenCaret, cs, 1)
  ('|' : '=' : cs) -> (Just TokenBarEq, cs, 2)
  ('|' : cs) -> (Just TokenBar, cs, 1)
  ('~' : cs) -> (Just TokenTilde, cs, 1)
  cs -> (Nothing, cs, 0)

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
lexNumSuffix num cont ('U' : cs) = \col -> cont (TokenUInt num) cs (col + 1)
lexNumSuffix num cont ('B' : cs) = \col -> cont (TokenByte num) cs (col + 1)
lexNumSuffix num cont cs = cont (TokenInt num) cs

lexNum :: (Token -> P a) -> P a
lexNum cont ('0' : 'x' : cs) =
  \col -> lexNumSuffix (parseHex num) cont rest (col + (length num + 2))
  where
    (num, rest) = span isHexDigit cs
lexNum cont cs =
  \col -> lexNumSuffix (read num) cont rest (col + length num)
  where
    (num, rest) = span isDigit cs

parseHex :: String -> Int
parseHex "" = 0
parseHex [c] = parseHexChar c
parseHex cs = parseHexChar (last cs) + 16 * parseHex (init cs)

parseHexChar :: Char -> Int
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
parseHexChar _ = undefined

lexVar :: (Token -> P a) -> P a
lexVar cont cs =
  case findKeywordToken keyword of
    Just token -> \col -> cont token rest (col + length keyword)
    Nothing -> \col -> cont (TokenIdentifier keyword) rest (col + length keyword)
  where
    (keyword, rest) = span isIdentifierChar cs

lexStr :: (Token -> P a) -> P a
lexStr cont (c : cs) = \col -> lexStrRec "" cont cs (col + 1)
lexStr cont [] = cont TokenEOF []

lexStrRec :: String -> (Token -> P a) -> P a
lexStrRec str cont ('\\' : '"' : cs) = \col -> lexStrRec ('"' : str) cont cs (col + 2)
lexStrRec str cont ('"' : cs) = \col -> cont (TokenString $ reverse str) cs (col + 1)
lexStrRec str cont (c : cs) = \col -> lexStrRec (c : str) cont cs (col + 1)
lexStrRec str cont [] = cont TokenEOF []

isIdentifierChar :: Char -> Bool
isIdentifierChar c = isAlphaNum c || c == '_'
