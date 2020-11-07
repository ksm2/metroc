module MetroLang.Compilation.Values where

import MetroLang.AST
import MetroLang.WebAssembly.AST

data Value = Value {
  dataType :: Type,
  wasmExpr :: Expr
} deriving (Show)

dataTypeToValtype :: Type -> Valtype
dataTypeToValtype (Primitive TDouble) = F64
dataTypeToValtype (Primitive TFloat) = F32
dataTypeToValtype (Primitive TLong) = I64
dataTypeToValtype _ = I32
