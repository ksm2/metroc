module MetroLang.WebAssembly.MemoryInstr where

import MetroLang.Model
import MetroLang.WebAssembly.AST

intToMaybe :: Int -> Maybe Integer
intToMaybe 0 = Nothing
intToMaybe x = Just $ toInteger x

loadInstr :: PrimitiveType -> Int -> Expr -> Expr
loadInstr TIntXS offset n1 = MemoryInstr "load8_s" I32 (intToMaybe offset) Nothing [n1]
loadInstr TByte offset n1 = MemoryInstr "load8_u" I32 (intToMaybe offset) Nothing [n1]
loadInstr TIntS offset n1 = MemoryInstr "load16_s" I32 (intToMaybe offset) Nothing [n1]
loadInstr TWord offset n1 = MemoryInstr "load16_u" I32 (intToMaybe offset) Nothing [n1]
loadInstr x offset n1
  | x == TInt || x == TUInt = MemoryInstr "load" I32 (intToMaybe offset) Nothing [n1]
  | x == TIntL || x == TUIntL = MemoryInstr "load" I64 (intToMaybe offset) Nothing [n1]
loadInstr _ _ _ = error "Cannot load with the given configuration."

storeInstr :: PrimitiveType -> Int -> Expr -> Expr -> Expr
storeInstr p offset n1 n2
  | p == TIntXS || p == TByte = MemoryInstr "store8" I32 (intToMaybe offset) Nothing [n1, n2]
  | p == TIntS || p == TWord = MemoryInstr "store16" I32 (intToMaybe offset) Nothing [n1, n2]
  | p == TInt || p == TUInt = MemoryInstr "store" I32 (intToMaybe offset) Nothing [n1, n2]
  | p == TIntL || p == TUIntL = MemoryInstr "store" I64 (intToMaybe offset) Nothing [n1, n2]
  | otherwise = error "Cannot store with the given configuration."
