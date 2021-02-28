module MetroLang.Lang.Token where

data Token
  = -- Keywords
    TokenAnd
  | TokenAs
  | TokenAssert
  | TokenClass
  | TokenConst
  | TokenElse
  | TokenEnum
  | TokenExport
  | TokenExtends
  | TokenFalse
  | TokenFn
  | TokenFor
  | TokenHide
  | TokenIf
  | TokenImpl
  | TokenImport
  | TokenInterface
  | TokenIs
  | TokenIt
  | TokenLet
  | TokenMatch
  | TokenNot
  | TokenNull
  | TokenOr
  | TokenReturn
  | TokenStatic
  | TokenTest
  | TokenThis
  | TokenTrue
  | TokenUnsafe
  | TokenWhile
  | TokenXor
  | -- Operators
    TokenAmp
  | TokenAmpEq
  | TokenBar
  | TokenBarEq
  | TokenCaret
  | TokenCaretEq
  | TokenColon
  | TokenColonEq
  | TokenComma
  | TokenDiv
  | TokenDivEq
  | TokenDot
  | TokenEq
  | TokenEqEq
  | TokenEqGt
  | TokenExclEq
  | TokenGt
  | TokenGtEq
  | TokenGtGt
  | TokenGtGtEq
  | TokenLBrace
  | TokenLBrack
  | TokenLParen
  | TokenLt
  | TokenLtEq
  | TokenLtLt
  | TokenLtLtEq
  | TokenLtRem
  | TokenLtRemEq
  | TokenMinus
  | TokenMinusEq
  | TokenMul
  | TokenMulEq
  | TokenPlus
  | TokenPlusEq
  | TokenQ
  | TokenQDot
  | TokenRBrace
  | TokenRBrack
  | TokenRem
  | TokenRemEq
  | TokenRemGt
  | TokenRemGtEq
  | TokenRParen
  | TokenTilde
  | TokenUnderscore
  | -- Literals
    TokenIdentifier String
  | TokenInt Int
  | TokenString String
  | -- Commands
    TokenEOS
  | TokenEOF
  deriving (Eq, Show)
