module MetroLang.Compilation.Context where

import Control.Monad.State (get, put, runState, State)
import Data.Map ((!), empty, insert, fromList, member, Map)
import MetroLang.AST
import MetroLang.Bytes (utf8Length)
import MetroLang.Types

data CompileContext = CompileContext {
  blockCtr :: Int,
  stringOffset :: Int,
  strings :: Map String Int,
  thisContext :: Maybe String,
  consts :: Map String Type,
  classes :: Map String ClassInfo,
  functions :: Map String FunctionInfo,
  scope :: [Scope]
} deriving Show

data ClassInfo = ClassInfo {
  fields :: Map String Int,
  classMethods :: Map String FunctionInfo
} deriving Show

data FunctionInfo = FunctionInfo {
  parameters :: [Type],
  returnDataType :: Type
} deriving Show

newtype Scope = Scope (Map String Type) deriving Show

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
        do  nextOffset <- return $ stringOffset + (utf8Length str) + 4
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

declareConst :: String -> Type -> Compiler ()
declareConst constName valueType =
  do  ctx@CompileContext { consts } <- get
      put $ ctx { consts = insert constName valueType consts }

hasConst :: String -> Compiler Bool
hasConst constName =
  do  CompileContext { consts } <- get
      return $ member constName consts

lookupConst :: String -> Compiler Type
lookupConst constName =
  do  CompileContext { consts } <- get
      return $ consts ! constName

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
  let (Method (MethodSignature methodName methodParams methodReturn) _) = m
      previousMap = readMethods ms
  in  insert methodName (createFunctionInfo methodParams methodReturn) previousMap

createFunctionInfo :: [Param] -> ReturnType -> FunctionInfo
createFunctionInfo params returnType =
  let paramTypes = map getParamType params
  in  FunctionInfo paramTypes returnType

getParamType :: Param -> Type
getParamType (Par _ t) = t

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
popScope :: Compiler Scope
popScope =
  do  ctx@CompileContext { scope } <- get
      h <- return $ head scope
      t <- return $ tail scope
      put $ ctx { scope = t }
      return h

scopeFromParams :: [Param] -> Map String Type
scopeFromParams [] = empty
scopeFromParams ((Par paramName paramType):ps) = insert paramName paramType $ scopeFromParams ps

declareVariable :: String -> Type -> Compiler ()
declareVariable varName varType=
  do  ctx@CompileContext { scope } <- get
      newScope <- declareVariableInScope varName varType $ head scope
      put $ ctx { scope = newScope:(tail scope) }

declareVariableInScope :: String -> Type -> Scope -> Compiler Scope
declareVariableInScope varName varType (Scope varMap) =
  if member varName varMap
  then error $ "Variable " ++ varName ++ " is already declared in this scope."
  else return $ Scope $ insert varName varType varMap

lookupVariableType :: String -> Compiler Type
lookupVariableType varName =
  do  CompileContext { scope } <- get
      return $ lookupVariableTypeInScopes varName scope

lookupVariableTypeInScopes :: String -> [Scope] -> Type
lookupVariableTypeInScopes varName [] = error $ "Could not find variable " ++ varName ++ " in scope."
lookupVariableTypeInScopes varName ((Scope x):xs) =
  if member varName x
  then (x ! varName)
  else lookupVariableTypeInScopes varName xs

builtInFunctions :: Map String FunctionInfo
builtInFunctions =
  fromList [ ("__storeByte", FunctionInfo [Primitive TInt, Primitive TByte] TVoid)
           , ("__loadByte", FunctionInfo [Primitive TInt] (Primitive TByte))
           , ("__storeInt", FunctionInfo [Primitive TInt, Primitive TInt] TVoid)
           , ("__loadInt", FunctionInfo [Primitive TInt] (Primitive TInt))
           , ("__storeLong", FunctionInfo [Primitive TInt, Primitive TLong] TVoid)
           , ("__loadLong", FunctionInfo [Primitive TInt] (Primitive TLong))
           , ("__allocate", FunctionInfo [Primitive TInt] (Primitive TInt))
           ]

-- | runCompiler executes the compilation of a module
runCompiler :: Compiler b -> (b, CompileContext)
runCompiler cb =
  let initialState = CompileContext {
                                      blockCtr = 0,
                                      stringOffset = 1024,
                                      strings = empty,
                                      thisContext = Nothing,
                                      consts = empty,
                                      classes = empty,
                                      functions = builtInFunctions,
                                      scope = []
                                    }
  in  runState cb initialState

type Compiler = State CompileContext
