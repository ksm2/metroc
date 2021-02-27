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
    ("hide", TokenHide),
    ("if", TokenIf),
    ("impl", TokenImpl),
    ("import", TokenImport),
    ("interface", TokenInterface),
    ("is", TokenIs),
    ("it", TokenIt),
    ("let", TokenLet),
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

findKeywordToken :: String -> Maybe Token
findKeywordToken keyword = lookup keyword keywords

isKeywordToken :: Token -> Bool
isKeywordToken token = any (\(_, t) -> t == token) keywords

isKeywordString :: String -> Bool
isKeywordString token = any (\(t, _) -> t == token) keywords
