module MetroLang.Types (sizeOf, calculateSizeOfClass) where

import MetroLang.AST

sizeOf :: Type -> Int
sizeOf t =
  case t of
    "String"  -> 4
    "Bool"    -> 1
    "Byte"    -> 1
    "UByte"   -> 1
    "Int"     -> 4
    "UInt"    -> 4
    "Long"    -> 8
    "ULong"   -> 8
    "Float"   -> 4
    "Double"  -> 8
    _ -> 0 -- TODO: Calculate size of other class

calculateSizeOfClass :: [Param] -> Int
calculateSizeOfClass [] = 0
calculateSizeOfClass (param:params) = (calculateSizeOfParam param) + (calculateSizeOfClass params)

calculateSizeOfParam :: Param -> Int
calculateSizeOfParam (Par _ t) = sizeOf t
