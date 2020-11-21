module MetroLang.Compilation.Compile (compile) where

import Control.Monad (liftM)
import Data.Map (assocs, toAscList)
import MetroLang.AST as Metro (PrimitiveType (..), Type (..))
import qualified MetroLang.AST as Metro
import MetroLang.Compilation.Combinators
import MetroLang.Compilation.Context
import MetroLang.Compilation.Expressions
import MetroLang.Compilation.Values
import MetroLang.Types
import qualified MetroLang.WebAssembly.AST as WASM
import MetroLang.WebAssembly.MemoryInstr
import MetroLang.WebAssembly.Utils

compileModule :: Metro.Module -> Compiler WASM.Module
compileModule (Metro.Mod d) = liftM WASM.Mod $ declarations d

declarations :: [Metro.Declaration] -> Compiler [WASM.Declaration]
declarations = flatMany declaration

declaration :: Metro.Declaration -> Compiler [WASM.Declaration]
declaration (Metro.Import moduleName specifier) =
  do
    wasmImportName <- importName specifier
    wasmImportSpecifier <- importSpecifier specifier
    return [WASM.Import moduleName wasmImportName wasmImportSpecifier]
declaration (Metro.Const constName value) =
  do
    Value valueType valueExpr <- expr value
    declareConst constName valueType
    return [WASM.Global constName (WASM.Imut (dataTypeToValtype valueType)) valueExpr]
declaration (Metro.Enumeration _name _typeArgs _) = return []
declaration (Metro.Interface _name _typeArgs _ _) = return []
declaration (Metro.Class name _typeArgs pars _ _ body) =
  do
    maybeType <- strToTypeMaybe name
    thisCtx <- return $ maybe (Generic name []) id maybeType
    setThisContext thisCtx
    declareClass name (createClassInfo pars body)
    parsedBody <- classBody body
    constr <- constructor name pars
    return $ constr : parsedBody
declaration (Metro.Impl _ targetType body) =
  do
    setThisContext targetType
    enhanceClass (show targetType) (createClassInfo [] body)
    classBody body
declaration (Metro.Func fnSafety fnName fnParams fnReturn body) =
  do
    isUnsafe <- return $ fnSafety == Metro.Unsafe
    declareFunction fnName $ FunctionInfo True isUnsafe (map getParamType fnParams) fnReturn
    p <- params fnParams
    r <- returnType fnReturn
    bb <- fnBlock body fnParams
    return [WASM.Func fnName p r bb]

importName :: Metro.ImportSpecifier -> Compiler String
importName (Metro.FuncImport fnName _ _) = return fnName

importSpecifier :: Metro.ImportSpecifier -> Compiler WASM.ImportSpecifier
importSpecifier (Metro.FuncImport fnName fnParams fnReturn) =
  do
    declareFunction fnName $ FunctionInfo True False (map getParamType fnParams) fnReturn
    p <- params fnParams
    r <- returnType fnReturn
    return $ WASM.IFunc fnName p r

constructor :: WASM.Identifier -> [Metro.Param] -> Compiler WASM.Declaration
constructor name pars =
  do
    p <- params pars
    sizeOfClass <- return $ i32Const $ toInteger $ calculateSizeOfClass pars
    allocation <- return $ setLocal "___ptr" $ call "__allocate" [sizeOfClass]
    fieldAssigns <- many assignField pars
    body <- return $ [WASM.Local "___ptr" WASM.I32, allocation] ++ fieldAssigns ++ [getLocal "___ptr"]
    return $ WASM.Func name p (Just (WASM.Res WASM.I32)) body

assignField :: Metro.Param -> Compiler WASM.Expr
assignField (Metro.Par fieldName _) =
  do
    className <- requireThisContext
    fieldOffset <- getFieldOffset (show className) fieldName
    return $ storeInstr TInt fieldOffset (getLocal "___ptr") (getLocal fieldName)

-- Classes
classBody :: Metro.ClassBody -> Compiler [WASM.Declaration]
classBody (Metro.ClassBody decls) = classBodyDeclarations decls

classBodyDeclarations :: [Metro.ClassBodyDeclaration] -> Compiler [WASM.Declaration]
classBodyDeclarations = flatMany classBodyDeclaration

data Owner = Static | Instance

classBodyDeclaration :: Metro.ClassBodyDeclaration -> Compiler [WASM.Declaration]
classBodyDeclaration (Metro.Method m b) = method Instance m b
classBodyDeclaration (Metro.StaticMethod m b) = method Static m b
classBodyDeclaration _ = return []

method :: Owner -> Metro.MethodSignature -> Metro.Block -> Compiler [WASM.Declaration]
method o m@(Metro.MethodSignature _ _ methodParams _) b =
  do
    signature <- methodSignature o m
    bb <- fnBlock b methodParams
    return [signature bb]

methodSignature :: Owner -> Metro.MethodSignature -> Compiler ([WASM.Expr] -> WASM.Declaration)
methodSignature Instance (Metro.MethodSignature _safety name methodParams methodReturn) =
  do
    className <- requireThisContext
    thisParam <- return $ WASM.Par "this" WASM.I32
    pp <- params methodParams
    r <- returnType methodReturn
    return $ WASM.Func (methodName className name) (thisParam : pp) r
methodSignature Static (Metro.MethodSignature _safety name methodParams methodReturn) =
  do
    className <- requireThisContext
    pp <- params methodParams
    r <- returnType methodReturn
    return $ WASM.Func (methodName className name) pp r

methodName :: Show a => a -> [Char] -> [Char]
methodName className name = (show className) ++ "." ++ name

-- Statements
fnBlock :: Metro.Block -> [Metro.Param] -> Compiler [WASM.Expr]
fnBlock (Metro.Block b) p =
  do
    pushScope p
    x <- stmts b
    Scope found <- popScope
    ls <- return $ makeLocalStmts (filter (isNotParam p) (assocs found))
    return $ ls ++ x

isNotParam :: [Metro.Param] -> (Metro.Identifier, Metro.Type) -> Bool
isNotParam fnParams (varName, _) = not $ any (paramNameEquals varName) fnParams

paramNameEquals :: String -> Metro.Param -> Bool
paramNameEquals expected (Metro.Par actual _) = expected == actual

block :: Metro.Block -> Compiler [WASM.Expr]
block (Metro.Block b) = stmts b

stmts :: [Metro.Stmt] -> Compiler [WASM.Expr]
stmts = flatMany stmt

stmt :: Metro.Stmt -> Compiler [WASM.Expr]
stmt (Metro.IfStmt i) = ifStmt i
stmt (Metro.WhileStmt cond whileBlock) =
  do
    Value condType condExpr <- expr cond
    if condType /= Metro.Primitive TBool
      then error "The while condition must be of type Bool."
      else do
        whileLabel <- label "while"
        continueLabel <- label "continue"
        b <- block whileBlock
        condBr <- return $ brIf whileLabel condExpr
        return [WASM.Block whileLabel Nothing $ [WASM.Loop continueLabel $ condBr : b ++ [br continueLabel]]]
stmt (Metro.ReturnStmt e Nothing) =
  do
    value <- expr e
    wasmEx <- return $ wasmExpr value
    return [WASM.Return wasmEx]
stmt (Metro.ReturnStmt e (Just c)) =
  do
    l <- label "return"
    value <- expr e
    cond <- ifCond l c
    wasmEx <- return $ wasmExpr value
    return [WASM.Block l Nothing $ [cond, WASM.Return wasmEx]]
stmt (Metro.UnsafeStmt body) = block body
stmt (Metro.AssertStmt cond message) =
  do
    isEnabled <- assertionsEnabled
    if isEnabled
      then assertion cond message
      else return []
stmt (Metro.ExprStmt e) =
  do
    value <- expr e
    wasmEx <- return $ wasmExpr value
    if (dataType value) /= TVoid
      then return [dropInstr wasmEx]
      else return [wasmEx]

-- If
ifStmt :: Metro.If -> Compiler [WASM.Expr]
ifStmt (Metro.If cond thenBlock Nothing) = thenStmt cond thenBlock []
ifStmt (Metro.If cond thenBlock (Just e)) =
  do
    l <- label "else"
    t <- thenStmt cond thenBlock [br l]
    f <- elseStmt e
    return [WASM.Block l Nothing $ t ++ f]

thenStmt :: Metro.Expression -> Metro.Block -> [WASM.Expr] -> Compiler [WASM.Expr]
thenStmt cond thenBlock elseCond =
  do
    l <- label "if"
    c <- ifCond l cond
    b <- block thenBlock
    return [WASM.Block l Nothing $ (c : b) ++ elseCond]

elseStmt :: Metro.Else -> Compiler [WASM.Expr]
elseStmt (Metro.ElseStmt b) = block b
elseStmt (Metro.ElseIfStmt i) = ifStmt i

ifCond :: String -> Metro.Expression -> Compiler WASM.Expr
ifCond i cond =
  do
    value <- expr cond
    case value of
      Value (Primitive TBool) wasmCond -> return $ brIf i $ i32Eqz wasmCond
      _ -> error "The if condition must be of type Bool."

assertion :: Metro.Expression -> String -> Compiler [WASM.Expr]
assertion cond message =
  do
    l <- label "assert"
    Value condType condExpr <- expr cond
    if condType /= Primitive TBool
      then error "Assertion condition must be a Bool."
      else do
        assertCond <- return $ brIf l condExpr
        ptr <- registerString message
        return [WASM.Block l Nothing [assertCond, call "printErr" [i32Const (toInteger ptr)], WASM.Instr "unreachable" []]]

params :: [Metro.Param] -> Compiler [WASM.Param]
params = many param

param :: Metro.Param -> Compiler WASM.Param
param (Metro.Par i t) = return $ WASM.Par i $ dataTypeToValtype t

returnType :: Metro.ReturnType -> Compiler WASM.ReturnType
returnType TVoid = return Nothing
returnType rt = return $ Just $ WASM.Res $ dataTypeToValtype rt

makeLocalStmts :: [(Metro.Identifier, Metro.Type)] -> [WASM.Expr]
makeLocalStmts = map makeLocalStmt

makeLocalStmt :: (Metro.Identifier, Metro.Type) -> WASM.Expr
makeLocalStmt (i, dt) = WASM.Local i $ dataTypeToValtype dt

compiling :: (a -> Compiler WASM.Module) -> Bool -> a -> WASM.Module
compiling cab enableAssertions a =
  let cb = cab a
      (b, cs) = runCompiler enableAssertions cb
   in injectStrings (toAscList (strings cs)) b

injectStrings :: [(String, Int)] -> WASM.Module -> WASM.Module
injectStrings [] m = m
injectStrings ((str, pos) : xs) m = injectStrings xs $ injectData pos str m

compile :: Bool -> Metro.Module -> WASM.Module
compile = compiling compileModule
