module MetroLang.WebAssembly.MemoryInstr where

import MetroLang.WebAssembly.AST

data Sign = Signed | Unsigned

intToMaybe :: Int -> Maybe Integer
intToMaybe 0 = Nothing
intToMaybe x = Just $ toInteger x

loadInstr :: Int -> Sign -> Int -> Expr -> Expr
loadInstr 8 Signed offset n1 = MemoryInstr "load8_s" I32 (intToMaybe offset) Nothing [n1]
loadInstr 8 Unsigned offset n1 = MemoryInstr "load8_u" I32 (intToMaybe offset) Nothing [n1]
loadInstr 16 Signed offset n1 = MemoryInstr "load16_s" I32 (intToMaybe offset) Nothing [n1]
loadInstr 16 Unsigned offset n1 = MemoryInstr "load16_u" I32 (intToMaybe offset) Nothing [n1]
loadInstr 32 _ offset n1 = MemoryInstr "load" I32 (intToMaybe offset) Nothing [n1]
loadInstr 64 _ offset n1 = MemoryInstr "load" I64 (intToMaybe offset) Nothing [n1]
loadInstr _ _ _ _ = error "Cannot load with the given configuration."

storeInstr :: Int -> Int -> Expr -> Expr -> Expr
storeInstr 8 offset n1 n2 = MemoryInstr "store8" I32 (intToMaybe offset) Nothing [n1, n2]
storeInstr 16 offset n1 n2 = MemoryInstr "store16" I32 (intToMaybe offset) Nothing [n1, n2]
storeInstr 32 offset n1 n2 = MemoryInstr "store" I32 (intToMaybe offset) Nothing [n1, n2]
storeInstr 64 offset n1 n2 = MemoryInstr "store" I64 (intToMaybe offset) Nothing [n1, n2]
storeInstr _ _ _ _ = error "Cannot store with the given configuration."
