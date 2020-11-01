module MetroLang.Compilation.Compile(compile) where

import Control.Monad (liftM)
import Data.Map (toAscList)
import qualified MetroLang.AST as Metro
import qualified MetroLang.WebAssembly.AST as WASM
import MetroLang.WebAssembly.Utils
import MetroLang.Compilation.Context
import MetroLang.Compilation.Combinators
import MetroLang.Compilation.Expressions
import MetroLang.Compilation.Values
import MetroLang.Types

compileModule :: Metro.Module -> Compiler WASM.Module
compileModule (Metro.Mod d) = liftM WASM.Mod $ declarations d

declarations :: [Metro.Declaration] -> Compiler [WASM.Declaration]
declarations = flatMany declaration

declaration :: Metro.Declaration -> Compiler [WASM.Declaration]
declaration (Metro.Import moduleName specifier) =
  do  wasmImportName <- importName specifier
      wasmImportSpecifier <- importSpecifier specifier
      return [WASM.Import moduleName wasmImportName wasmImportSpecifier]
declaration (Metro.Class name pars b) =
  do  setThisContext (Just name)
      declareClass name (createClassInfo pars)
      classBlockDeclarations <- classBlock b
      constr <- constructor name pars
      return $ constr:classBlockDeclarations
declaration (Metro.Func name pars b) =
  do  p <- params pars
      bb <- block b pars
      s <- stmtSeq $ (findLocals b) ++ bb
      return [WASM.Func name p Nothing s]

importName :: Metro.ImportSpecifier -> Compiler String
importName (Metro.FuncImport fnName _ _) = return fnName

importSpecifier :: Metro.ImportSpecifier -> Compiler WASM.ImportSpecifier
importSpecifier (Metro.FuncImport fnName fnParams fnReturn) =
  do  p <- params fnParams
      r <- returnType fnReturn
      return $ WASM.IFunc fnName p r

constructor :: WASM.Identifier -> [Metro.Param] -> Compiler WASM.Declaration
constructor name pars =
  do  p <- params pars
      sizeOfClass <- return $ i32Const $ toInteger $ calculateSizeOfClass pars
      allocation <- return $ WASM.Exp $ setLocal "___ptr" $ call "__allocate" [sizeOfClass]
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
      pp <- params pars
      bb <- block b pars
      s <- stmtSeq $ (findLocals b) ++ bb
      return $ WASM.Func (className ++ "." ++ name) (thisParam:pp) Nothing s

-- Statements
stmtSeq :: [WASM.Stmt] -> Compiler WASM.Stmt
stmtSeq s = return $ WASM.Seq s

block :: Metro.Block -> [Metro.Param] -> Compiler [WASM.Stmt]
block (Metro.Block b) p =
  do  pushScope p
      x <- stmts b
      popScope
      return x

stmts :: [Metro.Stmt] -> Compiler [WASM.Stmt]
stmts = many stmt

stmt :: Metro.Stmt -> Compiler WASM.Stmt
stmt (Metro.IfStmt i) = ifStmt i
stmt (Metro.ExprStmt e) =
  do  value <- expr e
      wasmEx <- return $ wasmExpr value
      if (dataType value) /= TVoid
      then return $ WASM.Seq [WASM.Exp wasmEx, WASM.Exp dropInstr]
      else return $ WASM.Exp wasmEx


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
      b <- block thenBlock []
      return $ WASM.Block l $ (c : b) ++ elseCond

elseStmt :: Metro.Else -> Compiler [WASM.Stmt]
elseStmt (Metro.ElseStmt b) = block b []
elseStmt (Metro.ElseIfStmt i) = (ifStmt i) >>= \x -> return [x]

ifCond :: String -> Metro.Expression -> Compiler WASM.Stmt
ifCond i cond =
  do  value <- expr cond
      case value of
        Value TBool wasmCond  -> return $ WASM.Exp $ brIf i wasmCond
        _                     -> error "The if condition must be of type Bool."

label :: String -> Compiler String
label s = incrCtr >>= \ctr -> return $ "___" ++ s ++ "_" ++ (show ctr)

params :: [Metro.Param] -> Compiler [WASM.Param]
params = many param

param :: Metro.Param -> Compiler WASM.Param
param (Metro.Par i t) = return $ WASM.Par i (valtype t)

returnType :: Metro.ReturnType -> Compiler WASM.ReturnType
returnType (Just rt) = return $ Just $ WASM.Res $ valtype rt
returnType Nothing = return Nothing

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
findLocals b = makeLocalStmts (filterDefs (flatMapStmtsToExprs b))

flatMapStmtsToExprs :: Metro.Block -> [Metro.Expression]
flatMapStmtsToExprs (Metro.Block statements) =
  statements >>= (\xs -> case xs of
    Metro.IfStmt (Metro.If _ x Nothing)   -> flatMapStmtsToExprs x
    Metro.IfStmt (Metro.If _ x (Just y))  -> (flatMapStmtsToExprs x) ++ (elseExprs y)
    Metro.ExprStmt x                      -> [x])

elseExprs :: Metro.Else -> [Metro.Expression]
elseExprs (Metro.ElseStmt n) = flatMapStmtsToExprs n
elseExprs (Metro.ElseIfStmt (Metro.If _ x Nothing)) = flatMapStmtsToExprs x
elseExprs (Metro.ElseIfStmt (Metro.If _ x (Just y))) = (flatMapStmtsToExprs x) ++ (elseExprs y)

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
