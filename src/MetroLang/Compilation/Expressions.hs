module MetroLang.Compilation.Expressions(exprs, expr) where

import qualified MetroLang.AST as Metro
import qualified MetroLang.WebAssembly.AST as WASM
import MetroLang.WebAssembly.Utils
import MetroLang.Compilation.Combinators
import MetroLang.Compilation.Context
import MetroLang.Compilation.Values

exprs :: [Metro.Expression] -> Compiler [Value]
exprs = many expr

trueValue :: Value
trueValue = Value TBool $ i32Const 1

falseValue :: Value
falseValue = Value TBool $ i32Const 0

expr :: Metro.Expression -> Compiler Value
expr (Metro.VariableExpr varName) =
  do  varType <- lookupVariableType varName
      return $ Value varType $ getLocal varName
expr (Metro.BooleanLiteral True) = return trueValue
expr (Metro.BooleanLiteral False) = return falseValue
expr (Metro.NumberLiteral n) = return $ Value TInt $ i32Const n
expr (Metro.StringLiteral l) =
  do  ptr <- registerString l
      return $ Value TString $ i32Const (toInteger ptr)
expr (Metro.NullLiteral) = return $ Value TNull $ i32Const 0
expr (Metro.ThisKeyword) =
  do  className <- requireThisContext
      return $ Value (TRef className) $ i32Const 0
expr (Metro.Unary op e) = unaryExpr op e
expr (Metro.Binary op e1 e2) = binaryExpr op e1 e2
expr (Metro.Call callee args) =
  do  a <- arguments args
      isConstructorCall <- classExists callee
      if isConstructorCall
      then return $ Value (TRef callee) $ call callee (map wasmExpr a)
      else return $ Value TInt $ call callee (map wasmExpr a) -- TODO: call can only return Int atm

unaryExpr :: Metro.UnaryOp -> Metro.Expression -> Compiler Value
unaryExpr op e = expr e >>= \value -> return $ unaryExprWasm op value

unaryExprWasm :: Metro.UnaryOp -> Value -> Value
unaryExprWasm Metro.Neg (Value TInt e) = Value TInt $ i32Sub (i32Const 0) e
unaryExprWasm Metro.Neg _ = error "Cannot negate the given value."
unaryExprWasm Metro.LogicalNot (Value TBool e) = Value TBool $ i32Eqz e
unaryExprWasm Metro.LogicalNot _ = error "Can only apply 'not' on a Bool."

binaryExpr :: Metro.BinOp -> Metro.Expression -> Metro.Expression -> Compiler Value
binaryExpr Metro.Definition e1 e2 = definitionExpr e1 e2
binaryExpr Metro.Assignment e1 e2 = assignment e1 e2

binaryExpr Metro.Chain Metro.ThisKeyword (Metro.VariableExpr fieldName) =
  do  className <- requireThisContext
      fieldOffset <- getFieldOffset className fieldName
      return $ Value TInt $ i32Load $ i32Add (getLocal "this") (i32Const $ toInteger fieldOffset)
      -- TODO: can only load Int
binaryExpr Metro.Chain obj (Metro.VariableExpr fieldName) =
  do  objValue <- expr obj
      fieldAccess objValue fieldName

binaryExpr Metro.Chain Metro.ThisKeyword (Metro.Call i2 args) =
  do  className <- requireThisContext
      thisAccess <- return $ Metro.VariableExpr "this"
      a <- arguments (prependArg thisAccess args)
      return $ Value TVoid $ call (className ++ "." ++ i2) (map wasmExpr a)
binaryExpr Metro.Chain obj (Metro.Call methodName args) =
  do  objValue <- expr obj
      argValues <- arguments args
      methodCall objValue methodName argValues

binaryExpr op e1 e2 =
  do  f1 <- expr e1
      f2 <- expr e2
      return $ binaryExprWasm op f1 f2

fieldAccess :: Value -> String -> Compiler Value
fieldAccess (Value TString obj) "length" = return $ Value TInt $ i32Load obj
fieldAccess (Value objType _) methodName = error $ "Unknown field " ++ methodName ++ " on primitive type " ++ show objType

methodCall :: Value -> String -> [Value] -> Compiler Value
methodCall (Value (TRef className) obj) methodName args =
  do  wasmArgs <- return $ map wasmExpr args
      return $ Value TVoid $ call (className ++ "." ++ methodName) $ obj:wasmArgs
methodCall (Value TString obj) "asInt" [] = return $ Value TInt $ obj
methodCall (Value objType _) methodName args = error $ "Unknown method " ++ methodName ++ argsToInfo args ++ " on primitive type " ++ show objType

argsToInfo :: [Value] -> String
argsToInfo vs = "(" ++ (joinArgs (map dataType vs)) ++ ")"

joinArgs :: (Show a) => [a] -> String
joinArgs [] = ""
joinArgs [element] = show element
joinArgs (x:xs) = (show x) ++ ", " ++ (joinArgs xs)

definitionExpr :: Metro.Expression -> Metro.Expression -> Compiler Value
definitionExpr (Metro.VariableExpr varName) ex =
  do  varValue <- expr ex
      declareVariable varName (dataType varValue)
      return $ Value TVoid $ setLocal varName (wasmExpr varValue)
definitionExpr _ _ = error "Bad variable declaration"

assignment :: Metro.Expression -> Metro.Expression -> Compiler Value
assignment (Metro.VariableExpr i) e2 =
  do  f2 <- expr e2
      return $ Value TVoid $ setLocal i (wasmExpr f2)
assignment _ _ = error "Bad variable assignment"

binaryExprWasm :: Metro.BinOp -> Value -> Value -> Value
binaryExprWasm Metro.Is _e1 _e2 = falseValue -- TODO!
binaryExprWasm Metro.Unequal (Value _ e1) (Value _ e2) = Value TBool $ i32Eqz (i32Eq e1 e2)
binaryExprWasm Metro.Equal (Value _ e1) (Value _ e2) = Value TBool $ i32Eq e1 e2
binaryExprWasm Metro.LogicalOr v1 v2 = boolExpr "or" v1 v2
binaryExprWasm Metro.LogicalAnd v1 v2 = boolExpr "and" v1 v2
binaryExprWasm Metro.Subtract v1 v2 = arithmeticExpr "sub" v1 v2
binaryExprWasm Metro.Add v1 v2 = arithmeticExpr "add" v1 v2
binaryExprWasm Metro.Modulo v1 v2 = arithmeticExpr "rem_s" v1 v2
binaryExprWasm Metro.Divide v1 v2 = arithmeticExpr "div_s" v1 v2
binaryExprWasm Metro.Multiply v1 v2 = arithmeticExpr "mul" v1 v2
binaryExprWasm _ _ _ = error "?. and . not implemented yet"
--binaryExpr Metro.OptChain e1 e2 = TODO!
--binaryExpr Metro.Chain e1 e2 = TODO!

boolExpr :: String -> Value -> Value -> Value
boolExpr op (Value TBool e1) (Value TBool e2) = Value TBool $ WASM.Method op WASM.I32 [e1, e2]
boolExpr op _ _ = error $ "Can only apply '" ++ op ++ "' on two Bools."

arithmeticExpr :: String -> Value -> Value -> Value
arithmeticExpr op (Value TInt e1) (Value TInt e2) = Value TInt $ WASM.Method op WASM.I32 [e1, e2]
arithmeticExpr op (Value TLong e1) (Value TLong e2) = Value TLong $ WASM.Method op WASM.I64 [e1, e2]
arithmeticExpr op _ _ = error $ "Cannot apply " ++ op ++ ": Types on left and right don't match."

arguments :: Metro.Arguments -> Compiler [Value]
arguments (Metro.Args e) = exprs e

prependArg :: Metro.Expression -> Metro.Arguments -> Metro.Arguments
prependArg item (Metro.Args e) = Metro.Args (item:e)
