module MetroLang.Compilation.Compile (compile) where

import Data.Map (assocs, toAscList)
import Data.Maybe
import MetroLang.Compilation.Combinators
import MetroLang.Compilation.Context
import MetroLang.Compilation.Expressions
import MetroLang.Compilation.Values
import MetroLang.Lang.Error
import MetroLang.Lang.Model (PrimitiveType (..), Type (..))
import qualified MetroLang.Lang.Model as Metro
import MetroLang.Types
import qualified MetroLang.WebAssembly.AST as WASM
import MetroLang.WebAssembly.MemoryInstr
import MetroLang.WebAssembly.Utils

compileModule :: Metro.Module -> Compiler WASM.Module
compileModule (Metro.Module d) = WASM.Mod <$> declarations d

declarations :: [Metro.Declaration] -> Compiler [WASM.Declaration]
declarations = flatMany declaration

declaration :: Metro.Declaration -> Compiler [WASM.Declaration]
declaration (Metro.ExternalDeclaration moduleName specifier) =
  do
    wasmImportName <- importName specifier
    wasmImportSpecifier <- external specifier
    return [WASM.Import moduleName wasmImportName wasmImportSpecifier]
declaration (Metro.ConstDeclaration constName value) =
  do
    Value valueType valueExpr <- expr value
    declareConst constName valueType
    return [WASM.Global constName (WASM.Imut (dataTypeToValtype valueType)) valueExpr]
declaration (Metro.EnumDeclaration _name _typeArgs _) = return []
declaration (Metro.InterfaceDeclaration _name _typeArgs _) = return []
declaration (Metro.ClassDeclaration name _typeArgs pars _ _ body) =
  do
    maybeType <- strToTypeMaybe name
    let thisCtx = fromMaybe (RefType name) maybeType
    setThisContext thisCtx
    declareClass name (createClassInfo pars body)
    parsedBody <- classBody body
    constr <- constructor name pars
    return $ constr : parsedBody
declaration (Metro.ImplDeclaration _ targetType body) =
  do
    setThisContext targetType
    enhanceClass (show targetType) (createClassInfo [] body)
    classBody body
declaration (Metro.FnDeclaration fnName fnSafety fnParams fnReturn body) =
  do
    let isUnsafe = fnSafety == Metro.Unsafe
    declareFunction fnName $ FunctionInfo True isUnsafe (map getParamType fnParams) fnReturn
    p <- params fnParams
    r <- returnType fnReturn
    bb <- fnBlock body fnParams
    let fnExport = Just fnName
    return [WASM.Func fnName fnExport p r bb]
declaration (Metro.TestDeclaration testName testStatements) = testDeclaration testName testStatements
declaration (Metro.ImportDeclaration _) = error "Imports not supported yet"
declaration (Metro.ExportDeclaration _) = error "Exports not supported yet"
declaration (Metro.HideDeclaration _) = error "Hides not supported yet"

importName :: Metro.External -> Compiler String
importName (Metro.FnExternal fnName _ _) = return fnName

external :: Metro.External -> Compiler WASM.ImportSpecifier
external (Metro.FnExternal fnName fnParams fnReturn) =
  do
    declareFunction fnName $ FunctionInfo True False (map getParamType fnParams) fnReturn
    p <- params fnParams
    r <- returnType fnReturn
    return $ WASM.IFunc fnName p r

constructor :: WASM.Identifier -> [Metro.Param] -> Compiler WASM.Declaration
constructor name pars =
  do
    p <- params pars
    let sizeOfClass = i32Const $ toInteger $ calculateSizeOfClass pars
    let allocation = setLocal "___ptr" $ call "__allocate" [sizeOfClass]
    fieldAssigns <- many assignField pars
    let body = [WASM.Local "___ptr" WASM.I32, allocation] ++ fieldAssigns ++ [getLocal "___ptr"]
    return $ WASM.Func name Nothing p (Just (WASM.Res WASM.I32)) body

assignField :: Metro.Param -> Compiler WASM.Expr
assignField (Metro.Param fieldName _) =
  do
    className <- requireThisContext
    fieldOffset <- getFieldOffset (show className) fieldName
    return $ storeInstr TInt fieldOffset (getLocal "___ptr") (getLocal fieldName)

-- Classes
classBody :: Metro.ClassBody -> Compiler [WASM.Declaration]
classBody = flatMany classElement

data Owner = Static | Instance

classElement :: Metro.ClassElement -> Compiler [WASM.Declaration]
classElement (Metro.Method m b) = method Instance m b
classElement (Metro.StaticMethod m b) = method Static m b
classElement _ = return []

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
    let thisParam = WASM.Par "this" WASM.I32
    pp <- params methodParams
    r <- returnType methodReturn
    return $ WASM.Func (methodName className name) Nothing (thisParam : pp) r
methodSignature Static (Metro.MethodSignature _safety name methodParams methodReturn) =
  do
    className <- requireThisContext
    pp <- params methodParams
    r <- returnType methodReturn
    return $ WASM.Func (methodName className name) Nothing pp r

methodName :: Show a => a -> String -> String
methodName className name = show className ++ "." ++ name

testDeclaration :: String -> [Metro.TestStatement] -> Compiler [WASM.Declaration]
testDeclaration testName = many (testStatement testName)

testStatement :: String -> Metro.TestStatement -> Compiler WASM.Declaration
testStatement testName (Metro.TestStatement description body) =
  do
    let l = testName ++ "$" ++ testDescriptionToIdentifier description
    b <- block body
    return $ WASM.Func l (Just description) [] Nothing b

testDescriptionToIdentifier :: String -> String
testDescriptionToIdentifier = map (\c -> if c == ' ' then '_' else c)

-- Statements
fnBlock :: Metro.Block -> [Metro.Param] -> Compiler [WASM.Expr]
fnBlock b p =
  do
    pushScope p
    x <- stmts b
    Scope found <- popScope
    let ls = makeLocalStatements (filter (isNotParam p) (assocs found))
    return $ ls ++ x

isNotParam :: [Metro.Param] -> (Metro.Identifier, Metro.Type) -> Bool
isNotParam fnParams (varName, _) = not $ any (paramNameEquals varName) fnParams

paramNameEquals :: String -> Metro.Param -> Bool
paramNameEquals expected (Metro.Param actual _) = expected == actual

block :: Metro.Block -> Compiler [WASM.Expr]
block = stmts

stmts :: [Metro.Statement] -> Compiler [WASM.Expr]
stmts = flatMany stmt

stmt :: Metro.Statement -> Compiler [WASM.Expr]
stmt (Metro.IfStatement i) = ifStatement i
stmt (Metro.WhileStatement cond whileBlock) =
  do
    Value condType condExpr <- expr cond
    if condType /= Metro.PrimitiveType TBool
      then error "The while condition must be of type Bool."
      else do
        whileLabel <- label "while"
        continueLabel <- label "continue"
        b <- block whileBlock
        let condBr = brIf whileLabel $ i32Eqz condExpr
        return [WASM.Block whileLabel Nothing [WASM.Loop continueLabel $ condBr : b ++ [br continueLabel]]]
stmt (Metro.ReturnStatement e Nothing) =
  do
    value <- expr e
    let wasmEx = wasmExpr value
    return [WASM.Return wasmEx]
stmt (Metro.ReturnStatement e (Just c)) =
  do
    l <- label "return"
    value <- expr e
    cond <- ifCond l c
    let wasmEx = wasmExpr value
    return [WASM.Block l Nothing [cond, WASM.Return wasmEx]]
stmt (Metro.UnsafeStatement body) = block body
stmt (Metro.AssertStatement cond message) =
  do
    isEnabled <- assertionsEnabled
    if isEnabled
      then assertion cond message
      else return []
stmt (Metro.ExpressionStatement e) =
  do
    value <- expr e
    let wasmEx = wasmExpr value
    if dataType value /= VoidType
      then return [dropInstr wasmEx]
      else return [wasmEx]
stmt (Metro.AssignStatement e1 e2) = definitionExpr e1 e2

definitionExpr :: Metro.Var -> Metro.Expression -> Compiler [WASM.Expr]
definitionExpr varName ex =
  do
    varValue <- expr ex
    declareVariable varName (dataType varValue)
    return [setLocal varName $ wasmExpr varValue]

-- If
ifStatement :: Metro.If -> Compiler [WASM.Expr]
ifStatement (Metro.If cond thenBlock Nothing) = thenStatement cond thenBlock []
ifStatement (Metro.If cond thenBlock (Just e)) =
  do
    l <- label "else"
    t <- thenStatement cond thenBlock [br l]
    f <- elseStatement e
    return [WASM.Block l Nothing $ t ++ f]

thenStatement :: Metro.Expression -> Metro.Block -> [WASM.Expr] -> Compiler [WASM.Expr]
thenStatement cond thenBlock elseCond =
  do
    l <- label "if"
    c <- ifCond l cond
    b <- block thenBlock
    return [WASM.Block l Nothing $ (c : b) ++ elseCond]

elseStatement :: Metro.Else -> Compiler [WASM.Expr]
elseStatement (Metro.Else b) = block b
elseStatement (Metro.ElseIf i) = ifStatement i

ifCond :: String -> Metro.Expression -> Compiler WASM.Expr
ifCond i cond =
  do
    value <- expr cond
    case value of
      Value (Metro.PrimitiveType TBool) wasmCond -> return $ brIf i $ i32Eqz wasmCond
      _ -> error "The if condition must be of type Bool."

assertion :: Metro.Expression -> String -> Compiler [WASM.Expr]
assertion cond message =
  do
    l <- label "assert"
    Value condType condExpr <- expr cond
    if condType /= PrimitiveType TBool
      then error "Assertion condition must be a Bool."
      else do
        let assertCond = brIf l condExpr
        ptr <- registerString message
        return [WASM.Block l Nothing [assertCond, call "printErr" [i32Const (toInteger ptr)], WASM.Instr "unreachable" []]]

params :: [Metro.Param] -> Compiler [WASM.Param]
params = many param

param :: Metro.Param -> Compiler WASM.Param
param (Metro.Param i t) = return $ WASM.Par i $ dataTypeToValtype t

returnType :: Metro.ReturnType -> Compiler WASM.ReturnType
returnType VoidType = return Nothing
returnType rt = return $ Just $ WASM.Res $ dataTypeToValtype rt

makeLocalStatements :: [(Metro.Identifier, Metro.Type)] -> [WASM.Expr]
makeLocalStatements = map makeLocalStatement

makeLocalStatement :: (Metro.Identifier, Metro.Type) -> WASM.Expr
makeLocalStatement (i, dt) = WASM.Local i $ dataTypeToValtype dt

compiling :: (a -> Compiler WASM.Module) -> Bool -> String -> a -> Either MetroError WASM.Module
compiling cab enableAssertions mainMethod a =
  let cb = cab a
      (b, cs) = runCompiler enableAssertions cb
   in fmap (injectStart mainMethod . injectStrings (toAscList (strings cs))) b

injectStart :: String -> WASM.Module -> WASM.Module
injectStart "" m = m
injectStart mainMethod (WASM.Mod m) = WASM.Mod $ m ++ [WASM.Start mainMethod]

injectStrings :: [(String, Int)] -> WASM.Module -> WASM.Module
injectStrings [] m = m
injectStrings ((str, pos) : xs) m = injectStrings xs $ injectData pos str m

compile :: Bool -> String -> Metro.Module -> Either MetroError WASM.Module
compile = compiling compileModule
