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

unsigned :: PrimitiveType -> PrimitiveType
unsigned TIntXS = TByte
unsigned TByte = TIntXS
unsigned TIntS = TWord
unsigned TWord = TIntS
unsigned TInt = TUInt
unsigned TUInt = TInt
unsigned TIntL = TUIntL
unsigned TUIntL = TIntL
unsigned e = e

isSignedType :: PrimitiveType -> Bool
isSignedType TByte = False
isSignedType TWord = False
isSignedType TUInt = False
isSignedType TUIntL = False
isSignedType _ = True

isUnsignedType :: PrimitiveType -> Bool
isUnsignedType = not . isSignedType
