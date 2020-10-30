module MetroLang.Compilation.Compile(compile) where

import Control.Monad (liftM)
import Data.Map (toAscList)
import qualified MetroLang.AST as Metro
import qualified MetroLang.WebAssembly.AST as WASM
import MetroLang.WebAssembly.Utils
import MetroLang.Compilation.Context
import MetroLang.Compilation.Combinators
import MetroLang.Compilation.Expressions
import MetroLang.Types

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

param :: Metro.Param -> Compiler WASM.Param
param (Metro.Par i t) = return $ WASM.Par i (valtype t)

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


compiling :: (a -> Compiler WASM.Module) -> a -> WASM.Module
compiling cab a =
  let cb = cab a
      (b, cs) = runCompiler cb
  in  injectStrings (toAscList (strings cs)) b

injectStrings :: [(String, Int)] -> WASM.Module -> WASM.Module
injectStrings [] m = m
injectStrings ((str, pos):xs) m = injectStrings xs $ injectData pos str m

compile :: Metro.Module -> WASM.Module
compile = compiling compileModule
