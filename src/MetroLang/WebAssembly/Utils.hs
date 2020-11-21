module MetroLang.WebAssembly.Utils
  ( injectData,
    br,
    brIf,
    call,
    dropInstr,
    select,
    getLocal,
    setLocal,
    getGlobal,
    setGlobal,
    i32Const,
    i32Eqz,
    i32Eq,
    i32Add,
    i32Mul,
    i32RemS,
    i32RemU,
    i32And,
    i32Or,
    i32Xor,
    i32Sub,
    i32Shl,
    i32Shru,
    i32WrapI64,
    i64Const,
    i64Xor,
    i64ExtendI32S,
    i64ExtendI32U,
  )
where

import qualified MetroLang.Bytes as Bytes
import MetroLang.WebAssembly.AST

injectDeclaration :: Declaration -> Module -> Module
injectDeclaration x (Mod xs) = Mod (x : xs)

injectData :: Int -> String -> Module -> Module
injectData i str m =
  let e = Method "const" I32 [Lit (toInteger i)]
      strBytes = Bytes.stringToBytes str
      len = length strBytes
      bytes = (Bytes.int32ToBytes len) ++ strBytes
   in injectDeclaration (Data e bytes) m

-- Helpers
br :: Identifier -> Expr
br i = Instr "br" [Var i]

brIf :: Identifier -> Expr -> Expr
brIf i cond = Instr "br_if" [Var i, cond]

call :: Identifier -> [Expr] -> Expr
call i args = Instr "call" $ (Var i) : args

select :: Expr -> Expr -> Expr -> Expr
select left right cond = Select left right cond

dropInstr :: Expr -> Expr
dropInstr expr = Instr "drop" [expr]

getLocal :: Identifier -> Expr
getLocal i = Instr "get_local" [Var i]

setLocal :: Identifier -> Expr -> Expr
setLocal i v = Instr "set_local" [Var i, v]

getGlobal :: Identifier -> Expr
getGlobal i = Instr "get_global" [Var i]

setGlobal :: Identifier -> Expr -> Expr
setGlobal i v = Instr "set_global" [Var i, v]

i32Const :: Integer -> Expr
i32Const num = Method "const" I32 [Lit num]

i32Eqz :: Expr -> Expr
i32Eqz cond = Method "eqz" I32 [cond]

i32Eq :: Expr -> Expr -> Expr
i32Eq n1 n2 = Method "eq" I32 [n1, n2]

i32And :: Expr -> Expr -> Expr
i32And n1 n2 = Method "and" I32 [n1, n2]

i32Or :: Expr -> Expr -> Expr
i32Or n1 n2 = Method "or" I32 [n1, n2]

i32Xor :: Expr -> Expr -> Expr
i32Xor n1 n2 = Method "xor" I32 [n1, n2]

i32Add :: Expr -> Expr -> Expr
i32Add n1 n2 = Method "add" I32 [n1, n2]

i32Sub :: Expr -> Expr -> Expr
i32Sub n1 n2 = Method "sub" I32 [n1, n2]

i32Mul :: Expr -> Expr -> Expr
i32Mul n1 n2 = Method "mul" I32 [n1, n2]

i32RemS :: Expr -> Expr -> Expr
i32RemS n1 n2 = Method "rem_s" I32 [n1, n2]

i32RemU :: Expr -> Expr -> Expr
i32RemU n1 n2 = Method "rem_u" I32 [n1, n2]

i32Shl :: Expr -> Expr -> Expr
i32Shl n1 n2 = Method "shl" I32 [n1, n2]

i32Shru :: Expr -> Expr -> Expr
i32Shru n1 n2 = Method "shr_u" I32 [n1, n2]

i32WrapI64 :: Expr -> Expr
i32WrapI64 n = Method "wrap_i64" I32 [n]

i64Const :: Integer -> Expr
i64Const num = Method "const" I64 [Lit num]

i64Xor :: Expr -> Expr -> Expr
i64Xor n1 n2 = Method "xor" I64 [n1, n2]

i64ExtendI32S :: Expr -> Expr
i64ExtendI32S n = Method "extend_i32_s" I64 [n]

i64ExtendI32U :: Expr -> Expr
i64ExtendI32U n = Method "extend_i32_u" I64 [n]
