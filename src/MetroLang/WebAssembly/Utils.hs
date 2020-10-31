module MetroLang.WebAssembly.Utils (
  injectData,
  merge,
  br,
  brIf,
  call,
  dropInstr,
  getLocal,
  setLocal,
  i32Const,
  i32Eqz,
  i32Eq,
  i32Add,
  i32Sub,
  i32Load,
  i32Store,
) where

import MetroLang.Bytes
import MetroLang.WebAssembly.AST

merge :: Module -> Module -> Module
merge (Mod d1) (Mod d2) = Mod (d1 ++ d2)

injectDeclaration :: Declaration -> Module -> Module
injectDeclaration x (Mod xs) = Mod (x:xs)

injectData :: Int -> String -> Module -> Module
injectData i str m =
  let e = Method "const" I32 [Lit (toInteger i)]
      len = length str - count '\\' str
  in  injectDeclaration (Data e (addString str (addInt32 (fromIntegral len) []))) m

count :: (Eq a) => a -> [a] -> Int
count ch = length . (filter (==ch))

-- Helpers
br :: Identifier -> Expr
br i = Instr "br" [Var i]
brIf :: Identifier -> Expr -> Expr
brIf i cond = Instr "br_if" [Var i, i32Eqz cond]
call :: Identifier -> [Expr] -> Expr
call i args = Instr "call" $ (Var i):args
dropInstr :: Expr
dropInstr = Instr "drop" []
getLocal :: Identifier -> Expr
getLocal i = Instr "get_local" [Var i]
setLocal :: Identifier -> Expr -> Expr
setLocal i v = Instr "set_local" [Var i, v]
i32Const :: Integer -> Expr
i32Const num = Method "const" I32 [Lit num]
i32Eqz :: Expr -> Expr
i32Eqz cond = Method "eqz" I32 [cond]
i32Eq :: Expr -> Expr -> Expr
i32Eq n1 n2 = Method "eq" I32 [n1, n2]
i32Add :: Expr -> Expr -> Expr
i32Add n1 n2 = Method "add" I32 [n1, n2]
i32Sub :: Expr -> Expr -> Expr
i32Sub n1 n2 = Method "sub" I32 [n1, n2]
i32Load :: Expr -> Expr
i32Load n1 = Method "load" I32 [n1]
i32Store :: Expr -> Expr -> Expr
i32Store n1 n2 = Method "store" I32 [n1, n2]
