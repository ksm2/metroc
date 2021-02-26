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
  | TokenVar String
  | TokenDot
  | TokenEq
  | TokenPlus
  | TokenMinus
  | TokenTimes
  | TokenDiv
  | TokenOB
  | TokenCB
  | TokenIdentifier String
  | TokenEOF
  deriving (Show)
