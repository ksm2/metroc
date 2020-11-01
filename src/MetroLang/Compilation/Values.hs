module MetroLang.Compilation.Values where

import MetroLang.WebAssembly.AST (Expr)

data DataType = TVoid
              | TInt
              | TLong
              | TBool
              | TString
              | TNull
              | TRef String

instance Eq DataType where
  (TRef c1) == (TRef c2) = c1 == c2
  TVoid == TVoid = True
  TInt == TInt = True
  TLong == TLong = True
  TBool == TBool = True
  TString == TString = True
  TNull == TNull = True
  _ == _ = False

instance Show DataType where
  show TVoid = "Void"
  show TInt = "Int"
  show TLong = "Long"
  show TBool = "Bool"
  show TString = "String"
  show TNull = "Null"
  show (TRef s) = s

data Value = Value {
  dataType :: DataType,
  wasmExpr :: Expr
} deriving (Show)

typeToDataType :: String -> DataType
typeToDataType "Int" = TInt
typeToDataType "Long" = TLong
typeToDataType "Bool" = TBool
typeToDataType "String" = TString
typeToDataType a = TRef a
