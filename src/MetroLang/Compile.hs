module MetroLang.Compile(compile) where

import qualified MetroLang.AST as Metro
import qualified MetroLang.WebAssembly.AST as WASM

declaration :: Metro.Declaration -> WASM.Declaration
declaration (Metro.Func name pars b) = WASM.Func name (map param pars) (stmtSeq (block b))

-- Statements
stmtSeq :: [WASM.Stmt] -> WASM.Stmt
stmtSeq s = WASM.Seq s

block :: Metro.Block -> [WASM.Stmt]
block (Metro.Block b) = map stmt b

stmt :: Metro.Stmt -> WASM.Stmt
stmt (Metro.IfStmt i) = ifStmt i
stmt (Metro.ExprStmt e) = WASM.Exp $ expr e


-- If
ifStmt :: Metro.If -> WASM.Stmt
ifStmt (Metro.If cond thenBlock Nothing) = thenStmt cond thenBlock []
ifStmt (Metro.If cond thenBlock (Just e)) =
  let label = "_else" in
    WASM.Block label $ (thenStmt cond thenBlock [WASM.Exp $ br label]) : (elseStmt e)

thenStmt :: Metro.Expression -> Metro.Block -> [WASM.Stmt] -> WASM.Stmt
thenStmt cond thenBlock elseCond =
  let label = "_if" in
    WASM.Block label $ ((ifCond label cond) : (block thenBlock)) ++ elseCond

elseStmt :: Metro.Else -> [WASM.Stmt]
elseStmt (Metro.ElseStmt b) = block b
elseStmt (Metro.ElseIfStmt i) = [ifStmt i]

ifCond :: String -> Metro.Expression -> WASM.Stmt
ifCond i cond = WASM.Exp $ brIf i (expr cond)


-- Expressions
expr :: Metro.Expression -> WASM.Expr
expr (Metro.VariableExpr i) = getLocal i
expr (Metro.BooleanLiteral True) = i32Const 1
expr (Metro.BooleanLiteral False) = i32Const 0
expr (Metro.NumberLiteral n) = i32Const n
expr (Metro.StringLiteral _) = i32Const 1024 -- TODO!
expr (Metro.NullLiteral) = i32Const 0
expr (Metro.Unary op e) = unaryExpr op e
expr (Metro.Binary op e1 e2) = binaryExpr op e1 e2
expr (Metro.Call i args) = call i (arguments args)

unaryExpr :: Metro.UnaryOp -> Metro.Expression -> WASM.Expr
unaryExpr Metro.Neg e = i32Sub (i32Const 0) (expr e)
unaryExpr Metro.LogicalNot e = i32Eqz (expr e)

binaryExpr :: Metro.BinOp -> Metro.Expression -> Metro.Expression -> WASM.Expr
binaryExpr Metro.Assignment (Metro.VariableExpr i) e2 = setLocal i (expr e2)
binaryExpr Metro.Definition (Metro.VariableExpr i) e2 = setLocal i (expr e2)
binaryExpr Metro.Is e1 e2 = i32Const 0 -- TODO!
binaryExpr Metro.Unequal e1 e2 = i32Eqz (i32Eq (expr e1) (expr e2))
binaryExpr Metro.Equal e1 e2 = i32Eq (expr e1) (expr e2)
binaryExpr Metro.LogicalOr e1 e2 = WASM.Method "or" WASM.I32 [expr e1, expr e2]
binaryExpr Metro.LogicalAnd e1 e2 = WASM.Method "and" WASM.I32 [expr e1, expr e2]
binaryExpr Metro.Subtract e1 e2 = WASM.Method "sub" WASM.I32 [expr e1, expr e2]
binaryExpr Metro.Add e1 e2 = WASM.Method "add" WASM.I32 [expr e1, expr e2]
binaryExpr Metro.Modulo e1 e2 = WASM.Method "rem_s" WASM.I32 [expr e1, expr e2]
binaryExpr Metro.Divide e1 e2 = WASM.Method "div_s" WASM.I32 [expr e1, expr e2]
binaryExpr Metro.Multiply e1 e2 = WASM.Method "mul" WASM.I32 [expr e1, expr e2]
--binaryExpr Metro.OptChain e1 e2 = TODO!
--binaryExpr Metro.Chain e1 e2 = TODO!

param :: Metro.Param -> WASM.Param
param (Metro.Par i t) = WASM.Par i (valtype t)

arguments :: Metro.Arguments -> [WASM.Expr]
arguments (Metro.Args e) = map expr e

-- Helpers
br i = WASM.Instr "br" [WASM.Var i]
brIf i cond = WASM.Instr "br_if" [WASM.Var i, i32Eqz cond]
call :: WASM.Identifier -> [WASM.Expr] -> WASM.Expr
call i args = WASM.Instr "call" $ (WASM.Var i):args
getLocal i = WASM.Instr "get_local" [WASM.Var i]
setLocal i v = WASM.Instr "set_local" [WASM.Var i, v]
i32Const num = WASM.Method "const" WASM.I32 [WASM.Lit num]
i32Eqz cond = WASM.Method "eqz" WASM.I32 [cond]
i32Eq :: WASM.Expr -> WASM.Expr -> WASM.Expr
i32Eq n1 n2 = WASM.Method "eq" WASM.I32 [n1, n2]
i32Sub n1 n2 = WASM.Method "sub" WASM.I32 [n1, n2]

-- Type conversion
valtype :: Metro.Type -> WASM.Valtype
valtype t = case t of
              "Bool" -> WASM.I32
              "Int" -> WASM.I32
              "UInt" -> WASM.I32
              "Long" -> WASM.I64
              "ULong" -> WASM.I64
              "Float" -> WASM.F32
              "Double" -> WASM.F64
              _ -> WASM.I32

compile :: Metro.Module -> WASM.Module
compile (Metro.Mod d) = WASM.Mod $ map declaration d
