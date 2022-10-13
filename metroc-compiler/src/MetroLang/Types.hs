module MetroLang.Types (sizeOf, calculateSizeOfClass) where

import MetroLang.Model

sizeOf :: Type -> Int
sizeOf (PrimitiveType TBool) = 1
sizeOf (PrimitiveType TIntXS) = 1
sizeOf (PrimitiveType TByte) = 1
sizeOf (PrimitiveType TInt) = 4
sizeOf (PrimitiveType TUInt) = 4
sizeOf (PrimitiveType TIntL) = 8
sizeOf (PrimitiveType TUIntL) = 8
sizeOf (PrimitiveType TFloat) = 4
sizeOf (PrimitiveType TFloatL) = 8
sizeOf (PrimitiveType TString) = 4
sizeOf _ = 0 -- TODO: Calculate size of other class

calculateSizeOfClass :: Params -> Int
calculateSizeOfClass = foldr ((+) . calculateSizeOfParam) 0

calculateSizeOfParam :: Param -> Int
calculateSizeOfParam (Param _ t) = sizeOf t
