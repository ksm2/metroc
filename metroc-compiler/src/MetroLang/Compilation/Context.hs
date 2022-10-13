module MetroLang.Compilation.Context where

import Control.Monad.Except
import Control.Monad.State (State, get, put, runState)
import Data.Map (Map, adjust, empty, fromList, insert, member, union, (!))
import MetroLang.Bytes (utf8Length)
import MetroLang.Lang.Error
import MetroLang.Lang.Model
import MetroLang.Types

data CompileContext = CompileContext
  { blockCtr :: Int,
    stringOffset :: Int,
    strings :: Map String Int,
    thisContext :: Type,
    consts :: Map String Type,
    classes :: Map String ClassInfo,
    functions :: Map String FunctionInfo,
    scope :: [Scope],
    enableAssertions :: Bool
  }
  deriving (Show)

data ClassInfo = ClassInfo
  { fields :: Map String Int,
    classMethods :: Map String FunctionInfo
  }
  deriving (Show)

data FunctionInfo = FunctionInfo
  { static :: Bool,
    unsafe :: Bool,
    parameters :: [Type],
    returnDataType :: Type
  }
  deriving (Show)

newtype Scope = Scope (Map String Type) deriving (Show)

label :: String -> Compiler String
label s = incrCtr >>= \ctr -> return $ "___" ++ s ++ "_" ++ show ctr

incrCtr :: Compiler Int
incrCtr =
  do
    ctx@CompileContext {blockCtr} <- get
    put $ ctx {blockCtr = blockCtr + 1}
    return blockCtr

registerString :: String -> Compiler Int
registerString str =
  do
    ctx@CompileContext {stringOffset, strings} <- get
    if member str strings
      then return $ strings ! str
      else do
        let nextOffset = stringOffset + utf8Length str + 4
        let inserted = insert str stringOffset strings
        put $ ctx {stringOffset = nextOffset, strings = inserted}
        return stringOffset

setThisContext :: Type -> Compiler ()
setThisContext thisContext =
  do
    ctx <- get
    put $ ctx {thisContext}

requireThisContext :: Compiler Type
requireThisContext =
  do
    CompileContext {thisContext} <- get
    case thisContext of
      VoidType -> error "You cannot use this in this context"
      f -> return f

declareConst :: String -> Type -> Compiler ()
declareConst constName valueType =
  do
    ctx@CompileContext {consts} <- get
    put $ ctx {consts = insert constName valueType consts}

hasConst :: String -> Compiler Bool
hasConst constName =
  do
    CompileContext {consts} <- get
    return $ member constName consts

lookupConst :: String -> Compiler Type
lookupConst constName =
  do
    CompileContext {consts} <- get
    return $ consts ! constName

declareClass :: String -> ClassInfo -> Compiler ()
declareClass className classInfo =
  do
    ctx@CompileContext {classes} <- get
    put $ ctx {classes = insert className classInfo classes}

enhanceClass :: String -> ClassInfo -> Compiler ()
enhanceClass className classInfo =
  do
    ctx@CompileContext {classes} <- get
    put $ ctx {classes = adjust (mergeClassInfo classInfo) className classes}

mergeClassInfo :: ClassInfo -> ClassInfo -> ClassInfo
mergeClassInfo ci1 ci2 =
  let ClassInfo fields1 methods1 = ci1
      ClassInfo fields2 methods2 = ci2
   in ClassInfo (fields1 `union` fields2) (methods1 `union` methods2)

createClassInfo :: Params -> ClassBody -> ClassInfo
createClassInfo classParams classBody =
  let fields = snd $ createClassFields $ reverse classParams
      methods = readClassBody classBody
   in ClassInfo fields methods

createClassFields :: [Param] -> (Int, Map String Int)
createClassFields [] = (0, empty)
createClassFields (Param fieldName t : params) =
  let (offset, existingMap) = createClassFields params
      newOffset = sizeOf t + offset
   in (newOffset, insert fieldName offset existingMap)

readClassBody :: ClassBody -> Map String FunctionInfo
readClassBody = readMethods

readMethods :: [ClassElement] -> Map String FunctionInfo
readMethods [] = empty
readMethods (m : ms) =
  case m of
    Method sig _ -> insertMethod False sig $ readMethods ms
    StaticMethod sig _ -> insertMethod True sig $ readMethods ms
    _ -> readMethods ms

insertMethod :: Bool -> MethodSignature -> Map Identifier FunctionInfo -> Map Identifier FunctionInfo
insertMethod isStatic (MethodSignature methodSafety methodName methodParams methodReturn) = insert methodName (createFunctionInfo isStatic (methodSafety == Unsafe) methodParams methodReturn)

createFunctionInfo :: Bool -> Bool -> Params -> ReturnType -> FunctionInfo
createFunctionInfo isStatic isUnsafe args returnType =
  let argTypes = map getParamType args
   in FunctionInfo isStatic isUnsafe argTypes returnType

getParamType :: Param -> Type
getParamType (Param _ t) = t

getFieldOffset :: String -> String -> Compiler Int
getFieldOffset className fieldName =
  do
    CompileContext {classes} <- get
    let classInfo = classes ! className
    return $ fields classInfo ! fieldName

lookupClassMethod :: String -> String -> Compiler FunctionInfo
lookupClassMethod className methodName =
  do
    classInfo <- lookupClass className
    if member methodName (classMethods classInfo)
      then return $ classMethods classInfo ! methodName
      else error $ "Class \"" ++ className ++ "\" has no method \"" ++ methodName ++ "\""

lookupClass :: String -> Compiler ClassInfo
lookupClass className =
  do
    CompileContext {classes} <- get
    if member className classes
      then return $ classes ! className
      else error $ "Class does not exist: \"" ++ className ++ "\""

classExists :: String -> Compiler Bool
classExists className =
  do
    CompileContext {classes} <- get
    return $ member className classes

-- | declareFunction declares a function in the application.
declareFunction :: String -> FunctionInfo -> Compiler ()
declareFunction fnName fnInfo =
  do
    ctx@CompileContext {functions} <- get
    put $ ctx {functions = insert fnName fnInfo functions}

-- | lookupFunction looks up a function declared in the application.
lookupFunction :: String -> Compiler FunctionInfo
lookupFunction fnName =
  do
    CompileContext {functions} <- get
    if member fnName functions
      then return $ functions ! fnName
      else error $ "Function does not exist: \"" ++ fnName ++ "\""

-- | pushScope adds a new empty scope on top of all scopes.
pushScope :: Params -> Compiler ()
pushScope p =
  let newScope = Scope $ scopeFromParams p
   in do
        ctx@CompileContext {scope} <- get
        put $ ctx {scope = newScope : scope}

-- | popScope removes the last declared scope.
popScope :: Compiler Scope
popScope =
  do
    ctx@CompileContext {scope} <- get
    let h = head scope
    let t = tail scope
    put $ ctx {scope = t}
    return h

scopeFromParams :: [Param] -> Map String Type
scopeFromParams [] = empty
scopeFromParams (Param paramName paramType : ps) = insert paramName paramType $ scopeFromParams ps

declareVariable :: String -> Type -> Compiler ()
declareVariable varName varType =
  do
    ctx@CompileContext {scope} <- get
    newScope <- declareVariableInScope varName varType $ head scope
    put $ ctx {scope = newScope : tail scope}

declareVariableInScope :: String -> Type -> Scope -> Compiler Scope
declareVariableInScope varName varType (Scope varMap) =
  if member varName varMap
    then error $ "Variable " ++ varName ++ " is already declared in this scope."
    else return $ Scope $ insert varName varType varMap

lookupVariableType :: String -> Compiler Type
lookupVariableType varName =
  do
    CompileContext {scope} <- get
    return $ lookupVariableTypeInScopes varName scope

lookupVariableTypeInScopes :: String -> [Scope] -> Type
lookupVariableTypeInScopes varName [] = error $ "Could not find variable " ++ varName ++ " in scope."
lookupVariableTypeInScopes varName (Scope x : xs) =
  if member varName x
    then x ! varName
    else lookupVariableTypeInScopes varName xs

primitiveTypes :: [PrimitiveType]
primitiveTypes = [minBound .. maxBound]

builtInTypes :: Map String ClassInfo
builtInTypes = fromList $ map createClassInfoForPrimitiveType primitiveTypes

createClassInfoForPrimitiveType :: PrimitiveType -> (String, ClassInfo)
createClassInfoForPrimitiveType p =
  (tail (show p), ClassInfo empty $ primitiveMethods p)

primitiveMethods :: PrimitiveType -> Map String FunctionInfo
primitiveMethods p =
  fromList
    [ ("load", FunctionInfo True True [PrimitiveType TInt] (PrimitiveType p)),
      ("store", FunctionInfo False True [PrimitiveType TInt] VoidType)
    ]

builtInFunctions :: Map String FunctionInfo
builtInFunctions =
  fromList
    [ ("__allocate", FunctionInfo True True [PrimitiveType TInt] (PrimitiveType TInt))
    ]

assertionsEnabled :: Compiler Bool
assertionsEnabled =
  do
    CompileContext {enableAssertions} <- get
    return enableAssertions

-- | runCompiler executes the compilation of a module
runCompiler :: Bool -> Compiler b -> (Either MetroError b, CompileContext)
runCompiler enableAssertions cb =
  let initialState =
        CompileContext
          { blockCtr = 0,
            stringOffset = 1024,
            strings = empty,
            thisContext = VoidType,
            consts = empty,
            classes = builtInTypes,
            functions = builtInFunctions,
            scope = [],
            enableAssertions
          }
   in runState (runExceptT cb) initialState

type Compiler a = ExceptT MetroError (State CompileContext) a
