module MetroLang.Compilation.Context where

import Control.Monad.State (get, put, runState, State)
import Data.Map ((!), empty, insert, member, Map)
import MetroLang.AST
import MetroLang.Types

data CompileContext = CompileContext {
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

createClassInfo :: [Param] -> ClassInfo
createClassInfo pars = ClassInfo $ snd $ createClassFields $ reverse pars

createClassFields :: [Param] -> (Int, Map String Int)
createClassFields [] = (0, empty)
createClassFields ((Par fieldName t):params) =
  let (offset, existingMap) = createClassFields params
      newOffset = (sizeOf t) + offset
  in (newOffset, insert fieldName offset existingMap)

getFieldOffset :: String -> String -> Compiler Int
getFieldOffset className fieldName =
  do  CompileContext { classes } <- get
      classInfo <- return (classes ! className)
      return $ (fields classInfo) ! fieldName

-- | runCompiler executes the compilation of a module
runCompiler :: Compiler b -> (b, CompileContext)
runCompiler cb =
  let initialState = CompileContext 0 2056 empty Nothing empty
  in  runState cb initialState

type Compiler = State CompileContext
