module MetroLang.Compilation.Values where

import MetroLang.WebAssembly.AST (Expr)

data DataType = TVoid
              | TInt
              | TLong
              | TBool
              | TString
              | TNull
              | TRef String
                deriving (Show)

data Value = Value {
  dataType :: DataType,
  wasmExpr :: Expr
} deriving (Show)
