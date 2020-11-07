module MetroLang.Compilation.Compile(compile) where

import Control.Monad (liftM)
import Data.Map (assocs, toAscList)
import MetroLang.AST as Metro (Type(..), PrimitiveType(..))
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
declaration (Metro.Const constName value) =
  do  Value valueType valueExpr <- expr value
      declareConst constName valueType
      return [WASM.Global constName (WASM.Imut (dataTypeToValtype valueType)) valueExpr]
declaration (Metro.Enumeration _name _typeArgs _) = return []
declaration (Metro.Interface _name _typeArgs _ _) = return []
declaration (Metro.Class name _typeArgs pars _ _ body) =
  do  setThisContext (Just name)
      declareClass name (createClassInfo pars body)
      classBlockDeclarations <- classBlock body
      constr <- constructor name pars
      return $ constr:classBlockDeclarations
declaration (Metro.Impl _ _ _) = return []
declaration (Metro.Func fnName fnParams fnReturn body) =
  do  declareFunction fnName $ FunctionInfo (map getParamType fnParams) fnReturn
      p <- params fnParams
      r <- returnType fnReturn
      bb <- fnBlock body fnParams
      s <- stmtSeq bb
      return [WASM.Func fnName p r s]

importName :: Metro.ImportSpecifier -> Compiler String
importName (Metro.FuncImport fnName _ _) = return fnName

importSpecifier :: Metro.ImportSpecifier -> Compiler WASM.ImportSpecifier
importSpecifier (Metro.FuncImport fnName fnParams fnReturn) =
  do  declareFunction fnName $ FunctionInfo (map getParamType fnParams) fnReturn
      p <- params fnParams
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
method (Metro.Method m@(Metro.MethodSignature _ methodParams _) b) =
  do  signature <- methodSignature m
      bb <- fnBlock b methodParams
      s <- stmtSeq bb
      return $ signature s

methodSignature :: Metro.MethodSignature -> Compiler (WASM.Stmt -> WASM.Declaration)
methodSignature (Metro.MethodSignature name methodParams methodReturn) =
  do  className <- requireThisContext
      thisParam <- return $ WASM.Par "this" WASM.I32
      pp <- params methodParams
      r <- returnType methodReturn
      return $ WASM.Func (className ++ "." ++ name) (thisParam:pp) r

-- Statements
stmtSeq :: [WASM.Stmt] -> Compiler WASM.Stmt
stmtSeq s = return $ WASM.Seq s

fnBlock :: Metro.Block -> [Metro.Param] -> Compiler [WASM.Stmt]
fnBlock (Metro.Block b) p =
  do  pushScope p
      x <- stmts b
      Scope found <- popScope
      ls <- return $ makeLocalStmts (filter (isNotParam p) (assocs found))
      return $ ls ++ x

isNotParam :: [Metro.Param] -> (Metro.Identifier, Metro.Type) -> Bool
isNotParam fnParams (varName, _) = not $ any (paramNameEquals varName) fnParams

paramNameEquals :: String -> Metro.Param -> Bool
paramNameEquals expected (Metro.Par actual _) = expected == actual

block :: Metro.Block -> Compiler [WASM.Stmt]
block (Metro.Block b) = stmts b

stmts :: [Metro.Stmt] -> Compiler [WASM.Stmt]
stmts = many stmt

stmt :: Metro.Stmt -> Compiler WASM.Stmt
stmt (Metro.IfStmt i) = ifStmt i
stmt (Metro.WhileStmt cond whileBlock) =
  do  Value condType condExpr <- expr cond
      if condType /= Metro.Primitive TBool
      then  error "The while condition must be of type Bool."
      else  do  whileLabel <- label "while"
                b <- block whileBlock
                condBr <- return $ WASM.Exp $ brIf whileLabel condExpr
                return $ WASM.Block whileLabel $ [WASM.Loop $ condBr:b]

stmt (Metro.ReturnStmt e) =
  do  value <- expr e
      wasmEx <- return $ wasmExpr value
      return $ WASM.Exp wasmEx
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
      b <- block thenBlock
      return $ WASM.Block l $ (c : b) ++ elseCond

elseStmt :: Metro.Else -> Compiler [WASM.Stmt]
elseStmt (Metro.ElseStmt b) = block b
elseStmt (Metro.ElseIfStmt i) = (ifStmt i) >>= \x -> return [x]

ifCond :: String -> Metro.Expression -> Compiler WASM.Stmt
ifCond i cond =
  do  value <- expr cond
      case value of
        Value (Primitive TBool) wasmCond  -> return $ WASM.Exp $ brIf i wasmCond
        _                     -> error "The if condition must be of type Bool."

label :: String -> Compiler String
label s = incrCtr >>= \ctr -> return $ "___" ++ s ++ "_" ++ (show ctr)

params :: [Metro.Param] -> Compiler [WASM.Param]
params = many param

param :: Metro.Param -> Compiler WASM.Param
param (Metro.Par i t) = return $ WASM.Par i $ dataTypeToValtype t

returnType :: Metro.ReturnType -> Compiler WASM.ReturnType
returnType TVoid = return Nothing
returnType rt = return $ Just $ WASM.Res $ dataTypeToValtype rt

makeLocalStmts :: [(Metro.Identifier, Metro.Type)] -> [WASM.Stmt]
makeLocalStmts = map makeLocalStmt

makeLocalStmt :: (Metro.Identifier, Metro.Type) -> WASM.Stmt
makeLocalStmt (i, dt) = WASM.Local i $ dataTypeToValtype dt

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
