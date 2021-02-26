module MetroLang.Lang.Keywords where

import Data.List
import MetroLang.Lang.Token

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

isKeywordToken :: Token -> Bool
isKeywordToken token = any (\(_, t) -> t == token) keywords
