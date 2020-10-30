module MetroLang.Compile(compile) where

import Control.Monad (liftM)
import Control.Monad.State (get, put, runState, State)
import Data.Map ((!), empty, insert, member, toAscList, Map)
import qualified MetroLang.AST as Metro
import qualified MetroLang.WebAssembly.AST as WASM
import MetroLang.WebAssembly.Utils
import MetroLang.Types

type Compiler = State CompileState

compileModule :: Metro.Module -> Compiler WASM.Module
compileModule (Metro.Mod d) = liftM WASM.Mod $ declarations d

declarations :: [Metro.Declaration] -> Compiler [WASM.Declaration]
declarations = flatMany declaration

declaration :: Metro.Declaration -> Compiler [WASM.Declaration]
declaration (Metro.Class name pars b) =
  do  setThisContext (Just name)
      declareClass name (createClassInfo pars)
      classBlockDeclarations <- classBlock b
      constr <- constructor name pars
      return $ constr:classBlockDeclarations
declaration (Metro.Func name pars b) =
  do  p <- many param pars
      bb <- block b
      s <- stmtSeq $ (findLocals b) ++ bb
      return [WASM.Func name p Nothing s]

constructor :: WASM.Identifier -> [Metro.Param] -> Compiler WASM.Declaration
constructor name pars =
  do  p <- many param pars
      sizeOfClass <- return $ i32Const $ toInteger $ calculateSizeOfClass pars
      allocation <- return $ WASM.Exp $ setLocal "___ptr" $ call "__metro_alloc" [sizeOfClass]
      fieldAssigns <- many assignField pars
      body <- return $ [WASM.Local "___ptr" WASM.I32, allocation] ++ fieldAssigns ++ [WASM.Exp $ getLocal "___ptr"]
      return $ WASM.Func name p (Just (WASM.Res WASM.I32)) $ WASM.Seq body

assignField :: Metro.Param -> Compiler WASM.Stmt
assignField (Metro.Par fieldName _) =
  do  className <- requireThisContext
      fieldOffset <- getFieldOffset className fieldName
      return $ WASM.Exp $ i32Store (i32Add (getLocal "___ptr") (i32Const $ toInteger fieldOffset)) (getLocal fieldName)

-- Classes
classBlock :: Metro.ClassBlock -> Compiler [WASM.Declaration]
classBlock (Metro.ClassBlock ms) = methods ms

methods :: [Metro.Method] -> Compiler [WASM.Declaration]
methods = many method

method :: Metro.Method -> Compiler WASM.Declaration
method (Metro.Method name pars b) =
  do  className <- requireThisContext
      thisParam <- return $ WASM.Par "this" WASM.I32
      pp <- many param pars
      bb <- block b
      s <- stmtSeq $ (findLocals b) ++ bb
      return $ WASM.Func (className ++ "." ++ name) (thisParam:pp) Nothing s

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
  do  l <- label "else"
      t <- thenStmt cond thenBlock [WASM.Exp $ br l]
      f <- elseStmt e
      return $ WASM.Block l $ t : f

thenStmt :: Metro.Expression -> Metro.Block -> [WASM.Stmt] -> Compiler WASM.Stmt
thenStmt cond thenBlock elseCond =
  do  l <- label "if"
      c <- ifCond l cond
      b <- block thenBlock
      return $ WASM.Block l $ (c : b) ++ elseCond

elseStmt :: Metro.Else -> Compiler [WASM.Stmt]
elseStmt (Metro.ElseStmt b) = block b
elseStmt (Metro.ElseIfStmt i) = (ifStmt i) >>= \x -> return [x]

ifCond :: String -> Metro.Expression -> Compiler WASM.Stmt
ifCond i cond =
  do  wasmCond <- expr cond
      return $ WASM.Exp $ brIf i wasmCond

label :: String -> Compiler String
label s = incrCtr >>= \ctr -> return $ "___" ++ s ++ "_" ++ (show ctr)


-- Expressions
exprs :: [Metro.Expression] -> Compiler [WASM.Expr]
exprs = many expr

expr :: Metro.Expression -> Compiler WASM.Expr
expr (Metro.VariableExpr i) = return $ getLocal i
expr (Metro.BooleanLiteral True) = return $ i32Const 1
expr (Metro.BooleanLiteral False) = return $ i32Const 0
expr (Metro.NumberLiteral n) = return $ i32Const n
expr (Metro.StringLiteral l) =
  do  cs@CompileState { stringOffset, strings } <- get
      if member l strings then
        return $ i32Const $ toInteger $ strings ! l
      else
        do  nextOffset <- return $ stringOffset + (length l) + 4
            inserted <- return $ insert l stringOffset strings
            put $ cs { stringOffset = nextOffset, strings = inserted }
            return $ i32Const (toInteger stringOffset)
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

param :: Metro.Param -> Compiler WASM.Param
param (Metro.Par i t) = return $ WASM.Par i (valtype t)

arguments :: Metro.Arguments -> Compiler [WASM.Expr]
arguments (Metro.Args e) = exprs e

prependArg :: Metro.Expression -> Metro.Arguments -> Metro.Arguments
prependArg item (Metro.Args e) = Metro.Args (item:e)

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

flatMany :: (b -> Compiler [a]) -> [b] -> Compiler [a]
flatMany _ [] = return []
flatMany singleCompiler (x:xs) =
  do  compiled <- singleCompiler x
      rest <- flatMany singleCompiler xs
      return $ compiled ++ rest

data CompileState = CompileState {
  blockCtr :: Int,
  stringOffset :: Int,
  strings :: Map String Int,
  thisContext :: Maybe String,
  classes :: Map String ClassInfo
} deriving Show

data ClassInfo = ClassInfo {
  fields :: Map String Int
} deriving Show

incrCtr :: Compiler Int
incrCtr =
  do cs@CompileState { blockCtr } <- get
     put $ cs { blockCtr = blockCtr + 1 }
     return blockCtr

setThisContext :: Maybe String -> Compiler ()
setThisContext thisContext =
  do cs <- get
     put $ cs { thisContext }

requireThisContext :: Compiler String
requireThisContext =
  do  CompileState { thisContext } <- get
      case thisContext of Just f  -> return f
                          _       -> error "You cannot use this in this context"

declareClass :: String -> ClassInfo -> Compiler ()
declareClass className classInfo =
  do  cs@CompileState { classes } <- get
      put $ cs { classes = insert className classInfo classes }

createClassInfo :: [Metro.Param] -> ClassInfo
createClassInfo pars = ClassInfo $ snd $ createClassFields $ reverse pars

createClassFields :: [Metro.Param] -> (Int, Map String Int)
createClassFields [] = (0, empty)
createClassFields ((Metro.Par fieldName t):params) =
  let (offset, existingMap) = createClassFields params
      newOffset = (sizeOf t) + offset
  in (newOffset, insert fieldName offset existingMap)

getFieldOffset :: String -> String -> Compiler Int
getFieldOffset className fieldName =
  do  CompileState { classes } <- get
      classInfo <- return (classes ! className)
      return $ (fields classInfo) ! fieldName

compiling :: (a -> Compiler WASM.Module) -> a -> WASM.Module
compiling cab a =
  let initialState = CompileState 0 2056 empty Nothing empty
      cb = cab a
      (b, cs) = runState cb initialState
  in  injectStrings (toAscList (strings cs)) b

injectStrings :: [(String, Int)] -> WASM.Module -> WASM.Module
injectStrings [] m = m
injectStrings ((str, pos):xs) m = injectStrings xs $ injectData pos str m

compile :: Metro.Module -> WASM.Module
compile = compiling compileModule
