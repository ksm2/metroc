module MetroLang.Compilation.Values where

import MetroLang.AST
import MetroLang.WebAssembly.AST

data Value = Value
  { dataType :: Type,
    wasmExpr :: Expr
  }
  deriving (Show)

dataTypeToValtype :: Type -> Valtype
dataTypeToValtype (Primitive TFloatL) = F64
dataTypeToValtype (Primitive TFloat) = F32
dataTypeToValtype (Primitive TIntL) = I64
dataTypeToValtype _ = I32
