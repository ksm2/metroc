module MetroLang.Lang.Lexer (lexer) where

import Data.Char
import Data.List
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
lexer cont ('.' : cs) = \col -> cont TokenDot cs (col + 1)

lexNum :: (Token -> P a) -> P a
lexNum cont cs = \col -> cont (TokenInt (read num)) rest (col + (length num))
  where
    (num, rest) = span isDigit cs

keywords :: [(String, Token)]
keywords =
  [ ("and", TokenAnd),
    ("as", TokenAs),
    ("assert", TokenAssert),
    ("class", TokenClass),
    ("const", TokenConst),
    ("else", TokenElse),
    ("enum", TokenEnum),
    ("export", TokenExport),
    ("extends", TokenExtends),
    ("false", TokenFalse),
    ("fn", TokenFn),
    ("for", TokenFor),
    ("if", TokenIf),
    ("it", TokenIt),
    ("impl", TokenImpl),
    ("import", TokenImport),
    ("interface", TokenInterface),
    ("is", TokenIs),
    ("match", TokenMatch),
    ("not", TokenNot),
    ("null", TokenNull),
    ("or", TokenOr),
    ("return", TokenReturn),
    ("static", TokenStatic),
    ("test", TokenTest),
    ("this", TokenThis),
    ("true", TokenTrue),
    ("unsafe", TokenUnsafe),
    ("while", TokenWhile),
    ("xor", TokenXor)
  ]

findKeywordToken :: String -> Maybe (String, Token)
findKeywordToken keyword = find (\(k, _) -> k == keyword) keywords

lexVar :: (Token -> P a) -> P a
lexVar cont cs =
  case findKeywordToken keyword of
    Just (_, token) -> \col -> cont token rest (col + (length keyword))
    Nothing -> \col -> cont (TokenIdentifier keyword) rest (col + (length keyword))
  where
    (keyword, rest) = span isAlpha cs
