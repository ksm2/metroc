module MetroLang.Lang.Token where

data Token
  = TokenAnd -- Keywords
  | TokenAs
  | TokenAssert
  | TokenClass
  | TokenConst
  | TokenElse
  | TokenEnum
  | TokenExport
  | TokenExtends
  | TokenExternal
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
  | TokenTBool -- Primitive Types
  | TokenTIntXS
  | TokenTByte
  | TokenTIntS
  | TokenTWord
  | TokenTInt
  | TokenTUInt
  | TokenTIntL
  | TokenTUIntL
  | TokenTFloat
  | TokenTFloatL
  | TokenTChar
  | TokenTString
  | TokenAmp -- Operators
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
  | TokenIdentifier String -- Literals
  | TokenInt Int
  | TokenUInt Int
  | TokenByte Int
  | TokenString String
  | TokenEOS -- Commands
  | TokenEOF
  deriving (Eq, Show)

isKeyword :: Token -> Bool
isKeyword =
  flip
    elem
    [ TokenAnd,
      TokenAs,
      TokenAssert,
      TokenClass,
      TokenConst,
      TokenElse,
      TokenEnum,
      TokenExport,
      TokenExtends,
      TokenExternal,
      TokenFalse,
      TokenFn,
      TokenFor,
      TokenHide,
      TokenIf,
      TokenImpl,
      TokenImport,
      TokenInterface,
      TokenIs,
      TokenIt,
      TokenLet,
      TokenMatch,
      TokenNot,
      TokenNull,
      TokenOr,
      TokenReturn,
      TokenStatic,
      TokenTest,
      TokenThis,
      TokenTrue,
      TokenUnsafe,
      TokenWhile
    ]
