module MetroLang.Lang.Token where

data Token
  = TokenAnd
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
  | TokenIf
  | TokenIt
  | TokenImpl
  | TokenImport
  | TokenInterface
  | TokenIs
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
  | TokenInt Int
  | TokenString String
  | TokenDot
  | TokenComma
  | TokenEq
  | TokenPlus
  | TokenMinus
  | TokenTimes
  | TokenDiv
  | TokenLT
  | TokenGT
  | TokenLParen
  | TokenRParen
  | TokenLBrace
  | TokenRBrace
  | TokenLBrack
  | TokenRBrack
  | TokenIdentifier String
  | TokenEOS
  | TokenEOF
  deriving (Eq, Show)
