module MetroLang.Compilation.Expressions(exprs, expr) where

import qualified MetroLang.AST as Metro
import qualified MetroLang.WebAssembly.AST as WASM
import MetroLang.WebAssembly.Utils
import MetroLang.Compilation.Combinators
import MetroLang.Compilation.Context

exprs :: [Metro.Expression] -> Compiler [WASM.Expr]
exprs = many expr

expr :: Metro.Expression -> Compiler WASM.Expr
expr (Metro.VariableExpr i) = return $ getLocal i
expr (Metro.BooleanLiteral True) = return $ i32Const 1
expr (Metro.BooleanLiteral False) = return $ i32Const 0
expr (Metro.NumberLiteral n) = return $ i32Const n
expr (Metro.StringLiteral l) =
  do  ptr <- registerString l
      return $ i32Const (toInteger ptr)
expr (Metro.NullLiteral) = return $ i32Const 0
expr (Metro.ThisKeyword) = return $ i32Const 0
expr (Metro.Unary op e) = unaryExpr op e
expr (Metro.Binary op e1 e2) = binaryExpr op e1 e2
expr (Metro.Call i args) =
  do  a <- arguments args
      return $ call i a

unaryExpr :: Metro.UnaryOp -> Metro.Expression -> Compiler WASM.Expr
unaryExpr Metro.Neg e = do f <- expr e; return $ i32Sub (i32Const 0) f
unaryExpr Metro.LogicalNot e = do f <- expr e; return $ i32Eqz f

binaryExpr :: Metro.BinOp -> Metro.Expression -> Metro.Expression -> Compiler WASM.Expr
binaryExpr Metro.Assignment e1 e2 = assignment e1 e2
binaryExpr Metro.Definition e1 e2 = assignment e1 e2
binaryExpr Metro.Chain (Metro.VariableExpr i1) (Metro.VariableExpr i2) = expr $ Metro.VariableExpr (i1 ++ "." ++ i2)
binaryExpr Metro.Chain (Metro.VariableExpr i1) (Metro.Call i2 args) = expr $ Metro.Call (i1 ++ "." ++ i2) args
binaryExpr Metro.Chain Metro.ThisKeyword (Metro.VariableExpr fieldName) =
  do  className <- requireThisContext
      fieldOffset <- getFieldOffset className fieldName
      return $ i32Load $ i32Add (getLocal "this") (i32Const $ toInteger fieldOffset)
binaryExpr Metro.Chain Metro.ThisKeyword (Metro.Call i2 args) =
  do  className <- requireThisContext
      thisAccess <- return $ Metro.VariableExpr "this"
      expr $ Metro.Call (className ++ "." ++ i2) (prependArg thisAccess args)
binaryExpr op e1 e2 =
  do  f1 <- expr e1
      f2 <- expr e2
      return $ binaryExprWasm op f1 f2

assignment :: Metro.Expression -> Metro.Expression -> Compiler WASM.Expr
assignment (Metro.VariableExpr i) e2 =
  do  f2 <- expr e2
      return $ setLocal i f2
assignment _ _ = error "Wrong assignment"

binaryExprWasm :: Metro.BinOp -> WASM.Expr -> WASM.Expr -> WASM.Expr
binaryExprWasm Metro.Is _e1 _e2 = i32Const 0 -- TODO!
binaryExprWasm Metro.Unequal e1 e2 = i32Eqz (i32Eq e1 e2)
binaryExprWasm Metro.Equal e1 e2 = i32Eq e1 e2
binaryExprWasm Metro.LogicalOr e1 e2 = WASM.Method "or" WASM.I32 [e1, e2]
binaryExprWasm Metro.LogicalAnd e1 e2 = WASM.Method "and" WASM.I32 [e1, e2]
binaryExprWasm Metro.Subtract e1 e2 = WASM.Method "sub" WASM.I32 [e1, e2]
binaryExprWasm Metro.Add e1 e2 = WASM.Method "add" WASM.I32 [e1, e2]
binaryExprWasm Metro.Modulo e1 e2 = WASM.Method "rem_s" WASM.I32 [e1, e2]
binaryExprWasm Metro.Divide e1 e2 = WASM.Method "div_s" WASM.I32 [e1, e2]
binaryExprWasm Metro.Multiply e1 e2 = WASM.Method "mul" WASM.I32 [e1, e2]
binaryExprWasm _ _ _ = error "?. and . not implemented yet"
--binaryExpr Metro.OptChain e1 e2 = TODO!
--binaryExpr Metro.Chain e1 e2 = TODO!

arguments :: Metro.Arguments -> Compiler [WASM.Expr]
arguments (Metro.Args e) = exprs e

prependArg :: Metro.Expression -> Metro.Arguments -> Metro.Arguments
prependArg item (Metro.Args e) = Metro.Args (item:e)
