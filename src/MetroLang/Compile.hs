module MetroLang.Compile(compile) where

import Control.Monad (liftM)
import Control.Monad.State (get, put, runState, State)
import qualified MetroLang.AST as Metro
import qualified MetroLang.WebAssembly.AST as WASM

data CompileState = CompileState Int deriving Show

type Compiler = State CompileState

compileModule :: Metro.Module -> Compiler WASM.Module
compileModule (Metro.Mod d) = liftM WASM.Mod $ declarations d

declarations :: [Metro.Declaration] -> Compiler [WASM.Declaration]
declarations = many declaration

declaration :: Metro.Declaration -> Compiler WASM.Declaration
declaration (Metro.Func name pars b) =
  do  p <- many param pars
      bb <- block b
      s <- stmtSeq $ (findLocals b) ++ bb
      return $ WASM.Func name p s

-- Statements
stmtSeq :: [WASM.Stmt] -> Compiler WASM.Stmt
stmtSeq s = return $ WASM.Seq s

block :: Metro.Block -> Compiler [WASM.Stmt]
block (Metro.Block b) = stmts b

stmts :: [Metro.Stmt] -> Compiler [WASM.Stmt]
stmts = many stmt

stmt :: Metro.Stmt -> Compiler WASM.Stmt
stmt (Metro.IfStmt i) = ifStmt i
stmt (Metro.ExprStmt e) = liftM WASM.Exp $ expr e


-- If
ifStmt :: Metro.If -> Compiler WASM.Stmt
ifStmt (Metro.If cond thenBlock Nothing) = thenStmt cond thenBlock []
ifStmt (Metro.If cond thenBlock (Just e)) =
  do  ctr <- incrCtr
      label <- return $ "___else_" ++ (show ctr)
      t <- thenStmt cond thenBlock [WASM.Exp $ br label]
      f <- elseStmt e
      return $ WASM.Block label $ t : f

thenStmt :: Metro.Expression -> Metro.Block -> [WASM.Stmt] -> Compiler WASM.Stmt
thenStmt cond thenBlock elseCond =
  do  ctr <- incrCtr
      label <- return $ "___if_" ++ (show ctr)
      c <- ifCond label cond
      b <- block thenBlock
      return $ WASM.Block label $ (c : b) ++ elseCond

elseStmt :: Metro.Else -> Compiler [WASM.Stmt]
elseStmt (Metro.ElseStmt b) = block b
elseStmt (Metro.ElseIfStmt i) = (ifStmt i) >>= \x -> return [x]

ifCond :: String -> Metro.Expression -> Compiler WASM.Stmt
ifCond i cond =
  do  wasmCond <- expr cond
      return $ WASM.Exp $ brIf i wasmCond


-- Expressions
exprs :: [Metro.Expression] -> Compiler [WASM.Expr]
exprs = many expr

expr :: Metro.Expression -> Compiler WASM.Expr
expr (Metro.VariableExpr i) = return $ getLocal i
expr (Metro.BooleanLiteral True) = return $ i32Const 1
expr (Metro.BooleanLiteral False) = return $ i32Const 0
expr (Metro.NumberLiteral n) = return $ i32Const n
expr (Metro.StringLiteral _) = return $ i32Const 1024 -- TODO!
expr (Metro.NullLiteral) = return $ i32Const 0
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

param :: Metro.Param -> Compiler WASM.Param
param (Metro.Par i t) = return $ WASM.Par i (valtype t)

arguments :: Metro.Arguments -> Compiler [WASM.Expr]
arguments (Metro.Args e) = exprs e

-- Helpers
br :: WASM.Identifier -> WASM.Expr
br i = WASM.Instr "br" [WASM.Var i]
brIf :: WASM.Identifier -> WASM.Expr -> WASM.Expr
brIf i cond = WASM.Instr "br_if" [WASM.Var i, i32Eqz cond]
call :: WASM.Identifier -> [WASM.Expr] -> WASM.Expr
call i args = WASM.Instr "call" $ (WASM.Var i):args
getLocal :: WASM.Identifier -> WASM.Expr
getLocal i = WASM.Instr "get_local" [WASM.Var i]
setLocal :: WASM.Identifier -> WASM.Expr -> WASM.Expr
setLocal i v = WASM.Instr "set_local" [WASM.Var i, v]
i32Const :: Integer -> WASM.Expr
i32Const num = WASM.Method "const" WASM.I32 [WASM.Lit num]
i32Eqz :: WASM.Expr -> WASM.Expr
i32Eqz cond = WASM.Method "eqz" WASM.I32 [cond]
i32Eq :: WASM.Expr -> WASM.Expr -> WASM.Expr
i32Eq n1 n2 = WASM.Method "eq" WASM.I32 [n1, n2]
i32Sub :: WASM.Expr -> WASM.Expr -> WASM.Expr
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

findLocals :: Metro.Block -> [WASM.Stmt]
findLocals (Metro.Block b) = makeLocalStmts (filterDefs (flatMapStmtsToExprs b))

flatMapStmtsToExprs :: [Metro.Stmt] -> [Metro.Expression]
flatMapStmtsToExprs statements = statements >>= (\xs -> case xs of Metro.ExprStmt x -> [x]; _ -> [])

filterDefs :: [Metro.Expression] -> [Metro.Identifier]
filterDefs expressions = expressions >>= (\xs -> case xs of Metro.Binary Metro.Definition (Metro.VariableExpr x) _ -> [x]; _ -> [])

makeLocalStmts :: [Metro.Identifier] -> [WASM.Stmt]
makeLocalStmts = map makeLocalStmt

makeLocalStmt :: Metro.Identifier -> WASM.Stmt
makeLocalStmt i = WASM.Local i WASM.I32

many :: (b -> Compiler a) -> [b] -> Compiler [a]
many _ [] = return []
many singleCompiler (x:xs) =
  do  compiled <- singleCompiler x
      rest <- many singleCompiler xs
      return $ compiled:rest

incrCtr :: Compiler Int
incrCtr =
  do (CompileState v) <- get
     put $ CompileState (v + 1)
     return v

compiling :: (a -> Compiler b) -> a -> b
compiling cab a =
  let initialState = CompileState 0 in
  let cb = cab a in
  let (b, _) = runState cb initialState in b

compile :: Metro.Module -> WASM.Module
compile = compiling compileModule
