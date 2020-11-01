module MetroLang.Compilation.Context where

import Control.Monad.State (get, put, runState, State)
import Data.Map ((!), empty, insert, fromList, member, Map)
import Data.Maybe (fromMaybe)
import MetroLang.AST
import MetroLang.Compilation.Values
import MetroLang.Types

data CompileContext = CompileContext {
  blockCtr :: Int,
  stringOffset :: Int,
  strings :: Map String Int,
  thisContext :: Maybe String,
  classes :: Map String ClassInfo,
  functions :: Map String FunctionInfo,
  scope :: [Scope]
} deriving Show

data ClassInfo = ClassInfo {
  fields :: Map String Int,
  classMethods :: Map String FunctionInfo
} deriving Show

data FunctionInfo = FunctionInfo {
  parameters :: [DataType],
  returnDataType :: DataType
} deriving Show

newtype Scope = Scope (Map String DataType) deriving Show

incrCtr :: Compiler Int
incrCtr =
  do  ctx@CompileContext { blockCtr } <- get
      put $ ctx { blockCtr = blockCtr + 1 }
      return blockCtr

registerString :: String -> Compiler Int
registerString str =
  do  ctx@CompileContext { stringOffset, strings } <- get
      if member str strings then
        return $ strings ! str
      else
        do  nextOffset <- return $ stringOffset + (length str) + 4
            inserted <- return $ insert str stringOffset strings
            put $ ctx { stringOffset = nextOffset, strings = inserted }
            return stringOffset

setThisContext :: Maybe String -> Compiler ()
setThisContext thisContext =
  do ctx <- get
     put $ ctx { thisContext }

requireThisContext :: Compiler String
requireThisContext =
  do  CompileContext { thisContext } <- get
      case thisContext of Just f  -> return f
                          _       -> error "You cannot use this in this context"

declareClass :: String -> ClassInfo -> Compiler ()
declareClass className classInfo =
  do  ctx@CompileContext { classes } <- get
      put $ ctx { classes = insert className classInfo classes }

createClassInfo :: [Param] -> ClassBlock -> ClassInfo
createClassInfo classParams (ClassBlock body) =
  let fields = snd $ createClassFields $ reverse classParams
      methods = readMethods body
  in ClassInfo fields methods

createClassFields :: [Param] -> (Int, Map String Int)
createClassFields [] = (0, empty)
createClassFields ((Par fieldName t):params) =
  let (offset, existingMap) = createClassFields params
      newOffset = (sizeOf t) + offset
  in (newOffset, insert fieldName offset existingMap)

readMethods :: [Method] -> Map String FunctionInfo
readMethods [] = empty
readMethods (m:ms) =
  let (Method methodName methodParams methodReturn _) = m
      previousMap = readMethods ms
  in  insert methodName (createFunctionInfo methodParams methodReturn) previousMap

createFunctionInfo :: [Param] -> ReturnType -> FunctionInfo
createFunctionInfo params returnType =
  let paramDataTypes = map getParamDataType params
      returnDataType = fromMaybe TVoid (fmap typeToDataType returnType)
  in  FunctionInfo paramDataTypes returnDataType

getParamDataType :: Param -> DataType
getParamDataType (Par _ t) = typeToDataType t

getFieldOffset :: String -> String -> Compiler Int
getFieldOffset className fieldName =
  do  CompileContext { classes } <- get
      classInfo <- return (classes ! className)
      return $ (fields classInfo) ! fieldName

lookupClassMethod :: String -> String -> Compiler FunctionInfo
lookupClassMethod className methodName =
  do  classInfo <- lookupClass className
      if member methodName (classMethods classInfo)
      then return $ (classMethods classInfo) ! methodName
      else error $ "Class \"" ++ className ++ "\" has no method \"" ++ methodName ++ "\""

lookupClass :: String -> Compiler ClassInfo
lookupClass className =
  do  CompileContext { classes } <- get
      if member className classes
      then return $ classes ! className
      else error $ "Class does not exist: \"" ++ className ++ "\""

classExists :: String -> Compiler Bool
classExists className =
  do  CompileContext { classes } <- get
      return $ member className classes

-- | declareFunction declares a function in the application.
declareFunction :: String -> FunctionInfo -> Compiler ()
declareFunction fnName fnInfo =
  do  ctx@CompileContext { functions } <- get
      put $ ctx { functions = insert fnName fnInfo functions }

-- | lookupFunction looks up a function declared in the application.
lookupFunction :: String -> Compiler FunctionInfo
lookupFunction fnName =
  do  CompileContext { functions } <- get
      if member fnName functions
      then return $ functions ! fnName
      else error $ "Function does not exist: \"" ++ fnName ++ "\""

-- | pushScope adds a new empty scope on top of all scopes.
pushScope :: [Param] -> Compiler ()
pushScope p =
  let newScope = Scope $ scopeFromParams p
  in  do  ctx@CompileContext { scope } <- get
          put $ ctx { scope = newScope:scope }

-- | popScope removes the last declared scope.
popScope :: Compiler ()
popScope =
  do  ctx@CompileContext { scope } <- get
      t <- return $ tail scope
      put $ ctx { scope = t }

scopeFromParams :: [Param] -> Map String DataType
scopeFromParams [] = empty
scopeFromParams ((Par paramName paramType):ps) = insert paramName (typeToDataType paramType) $ scopeFromParams ps

declareVariable :: String -> DataType -> Compiler ()
declareVariable varName varType=
  do  ctx@CompileContext { scope } <- get
      newScope <- declareVariableInScope varName varType $ head scope
      put $ ctx { scope = newScope:(tail scope) }

declareVariableInScope :: String -> DataType -> Scope -> Compiler Scope
declareVariableInScope varName varType (Scope varMap) =
  if member varName varMap
  then error $ "Variable " ++ varName ++ " is already declared in this scope."
  else return $ Scope $ insert varName varType varMap

lookupVariableType :: String -> Compiler DataType
lookupVariableType varName =
  do  CompileContext { scope } <- get
      return $ lookupVariableTypeInScopes varName scope

lookupVariableTypeInScopes :: String -> [Scope] -> DataType
lookupVariableTypeInScopes varName [] = error $ "Could not find variable " ++ varName ++ " in scope."
lookupVariableTypeInScopes varName ((Scope x):xs) =
  if member varName x
  then (x ! varName)
  else lookupVariableTypeInScopes varName xs

builtInFunctions :: Map String FunctionInfo
builtInFunctions =
  fromList [ ("__storeInt", FunctionInfo [TInt, TInt] TVoid)
           , ("__loadInt", FunctionInfo [TInt] TInt)
           , ("__allocate", FunctionInfo [TInt] TInt)
           ]

-- | runCompiler executes the compilation of a module
runCompiler :: Compiler b -> (b, CompileContext)
runCompiler cb =
  let initialState = CompileContext 0 2056 empty Nothing empty builtInFunctions []
  in  runState cb initialState

type Compiler = State CompileContext
