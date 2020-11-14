module MetroLang.Types (sizeOf, calculateSizeOfClass) where

import MetroLang.AST

sizeOf :: Type -> Int
sizeOf (Primitive TBool) = 1
sizeOf (Primitive TIntXS) = 1
sizeOf (Primitive TByte) = 1
sizeOf (Primitive TInt) = 4
sizeOf (Primitive TUInt) = 4
sizeOf (Primitive TIntL) = 8
sizeOf (Primitive TUIntL) = 8
sizeOf (Primitive TFloat) = 4
sizeOf (Primitive TDouble) = 8
sizeOf (Primitive TString) = 4
sizeOf _ = 0 -- TODO: Calculate size of other class

calculateSizeOfClass :: [Param] -> Int
calculateSizeOfClass [] = 0
calculateSizeOfClass (param : params) = (calculateSizeOfParam param) + (calculateSizeOfClass params)

calculateSizeOfParam :: Param -> Int
calculateSizeOfParam (Par _ t) = sizeOf t
