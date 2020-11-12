module MetroLang.Compilation.Expressions (exprs, expr) where

import MetroLang.AST as Metro (PrimitiveType (..), Type (..))
import qualified MetroLang.AST as Metro
import MetroLang.Compilation.Combinators
import MetroLang.Compilation.Context
import MetroLang.Compilation.Values
import MetroLang.Types
import qualified MetroLang.WebAssembly.AST as WASM
import MetroLang.WebAssembly.Utils

exprs :: [Metro.Expression] -> Compiler [Value]
exprs = many expr

trueValue :: Value
trueValue = Value (Primitive TBool) $ i32Const 1

falseValue :: Value
falseValue = Value (Primitive TBool) $ i32Const 0

expr :: Metro.Expression -> Compiler Value
expr (Metro.VariableExpr varName) =
  do
    isConst <- hasConst varName
    if isConst
      then do
        varType <- lookupConst varName
        return $ Value varType $ getGlobal varName
      else do
        varType <- lookupVariableType varName
        return $ Value varType $ getLocal varName
expr (Metro.BooleanLiteral True) = return trueValue
expr (Metro.BooleanLiteral False) = return falseValue
expr (Metro.NumberLiteral p n) = return $ Value (Primitive p) $ i32Const n
expr (Metro.StringLiteral l) =
  do
    ptr <- registerString l
    return $ Value (Primitive TString) $ i32Const (toInteger ptr)
expr (Metro.NullLiteral) = return $ Value TVoid $ i32Const 0
expr (Metro.ThisKeyword) =
  do
    classType <- requireThisContext
    return $ Value classType $ getLocal "this"
expr (Metro.Unary op e) = unaryExpr op e
expr (Metro.Binary op e1 e2) = binaryExpr op e1 e2
expr (Metro.Call callee args) =
  do
    a <- arguments args
    isConstructorCall <- classExists callee
    if isConstructorCall
      then return $ Value (Generic callee []) $ call callee (map wasmExpr a)
      else functionCall callee a
expr (Metro.ListAccess obj key) = listAccessExpr obj key

functionCall :: String -> [Value] -> Compiler Value
functionCall fnName args =
  do
    functionInfo <- lookupFunction fnName
    wasmArgs <- return $ checkFunctionSignature 1 fnName (parameters functionInfo) args
    return $ Value (returnDataType functionInfo) $ call fnName wasmArgs

unaryExpr :: Metro.UnaryOp -> Metro.Expression -> Compiler Value
unaryExpr op e = expr e >>= \value -> return $ unaryExprWasm op value

unaryExprWasm :: Metro.UnaryOp -> Value -> Value
unaryExprWasm Metro.Neg (Value (Primitive TInt) e) = Value (Primitive TInt) $ i32Sub (i32Const 0) e
unaryExprWasm Metro.Neg _ = error "Cannot negate the given value."
unaryExprWasm Metro.LogicalNot (Value (Primitive TBool) e) = Value (Primitive TBool) $ i32Eqz e
unaryExprWasm Metro.LogicalNot _ = error "Can only apply 'not' on a Bool."

binaryExpr :: Metro.BinOp -> Metro.Expression -> Metro.Expression -> Compiler Value
binaryExpr Metro.Definition e1 e2 = definitionExpr e1 e2
binaryExpr Metro.Assignment e1 e2 = assignment e1 e2
binaryExpr Metro.Chain obj (Metro.VariableExpr fieldName) =
  do
    objValue <- expr obj
    fieldAccess objValue fieldName
binaryExpr Metro.Chain obj (Metro.Call methodName args) =
  do
    objValue <- expr obj
    argValues <- arguments args
    methodCall objValue methodName argValues
binaryExpr op e1 e2 =
  do
    f1 <- expr e1
    f2 <- expr e2
    return $ binaryExprWasm op f1 f2

fieldAccess :: Value -> String -> Compiler Value
fieldAccess (Value (Primitive TUInt) obj) "lowUWord" = return $ Value (Primitive TUWord) $ toWord obj
fieldAccess (Value (Primitive TUInt) obj) "highUWord" = return $ Value (Primitive TUWord) $ i32Shru obj $ i32Const 16
fieldAccess (Value (Primitive TString) obj) "length" = return $ Value (Primitive TInt) $ i32Load obj
fieldAccess (Value (List _) obj) "length" = return $ Value (Primitive TUInt) $ i32Load obj
fieldAccess (Value (Generic className _) objExpr) fieldName =
  do
    fieldOffset <- getFieldOffset className fieldName
    return $ Value (Primitive TInt) $ i32Load $ i32Add objExpr (i32Const $ toInteger fieldOffset)
-- TODO: can only load Int
fieldAccess (Value objType _) methodName = error $ "Unknown field " ++ methodName ++ " on primitive type " ++ show objType

methodCall :: Value -> String -> [Value] -> Compiler Value
methodCall (Value (Primitive TUByte) obj) "toUWord" [] = return $ Value (Primitive TUWord) obj
methodCall (Value (Primitive TUByte) obj) "toUInt" [] = return $ Value (Primitive TUInt) obj
methodCall (Value (Primitive TUWord) obj) "toUInt" [] = return $ Value (Primitive TUInt) obj
methodCall (Value (Primitive TInt) obj) "toLong" [] = return $ Value (Primitive TLong) $ i64ExtendI32S obj
methodCall (Value (Primitive TInt) obj) "toWord" [] = return $ Value (Primitive TWord) $ toWord obj
methodCall (Value (Primitive TInt) obj) "toByte" [] = return $ Value (Primitive TByte) $ toByte obj
methodCall (Value (Primitive TUInt) obj) "toByte" [] = return $ Value (Primitive TByte) $ toByte obj
methodCall (Value (Primitive TString) obj) "toUByteList" [] = return $ Value (List (Primitive TUByte)) $ obj
methodCall (Value (Primitive TString) obj) "asInt" [] = return $ Value (Primitive TInt) $ obj
methodCall (Value objType obj) methodName args = classMethodCall (show objType) obj methodName args

--methodCall (Value objType _) methodName args = error $ "Unknown method " ++ methodName ++ argsToInfo args ++ " on primitive type " ++ show objType

classMethodCall :: String -> WASM.Expr -> String -> [Value] -> Compiler Value
classMethodCall className obj methodName args =
  do
    method <- lookupClassMethod className methodName
    wasmArgs <- return $ checkFunctionSignature 1 methodName (parameters method) args
    return $ Value (returnDataType method) $ call (className ++ "." ++ methodName) $ obj : wasmArgs

listAccessExpr :: Metro.Expression -> Metro.Expression -> Compiler Value
listAccessExpr obj key =
  do
    Value keyType keyExpr <- expr key
    if keyType /= Primitive TUInt
      then error "List access needs to be of type UInt."
      else do
        Value objType objExpr <- expr obj
        case objType of
          List listType -> return $ load listType $ i32Add (i32Const 4) $ i32Add objExpr $ i32Mul keyExpr $ i32Const $ toInteger (sizeOf listType)
          _ -> error "Can only access lists by index."

load :: Type -> WASM.Expr -> Value
load (Primitive TByte) n1 = Value (Primitive TByte) $ WASM.Method "load8_s" WASM.I32 [n1]
load (Primitive TUByte) n1 = Value (Primitive TUByte) $ WASM.Method "load8_u" WASM.I32 [n1]
load (Primitive TWord) n1 = Value (Primitive TWord) $ WASM.Method "load16_s" WASM.I32 [n1]
load (Primitive TUWord) n1 = Value (Primitive TUWord) $ WASM.Method "load16_u" WASM.I32 [n1]
load (Primitive TLong) n1 = Value (Primitive TLong) $ WASM.Method "load" WASM.I64 [n1]
load (Primitive TULong) n1 = Value (Primitive TULong) $ WASM.Method "load" WASM.I64 [n1]
load x n1 = Value x $ WASM.Method "load" WASM.I32 [n1]

checkFunctionSignature :: Int -> String -> [Metro.Type] -> [Value] -> [WASM.Expr]
checkFunctionSignature _ _ [] [] = []
checkFunctionSignature no fnName (p : params) (a : args) =
  let Value dt ex = a
   in if p == dt
        then ex : (checkFunctionSignature (no + 1) fnName params args)
        else error $ "The " ++ (ordnum no) ++ " parameter type of \"" ++ fnName ++ "\" does not match: Expected " ++ (show p) ++ ", but was " ++ (show dt)
checkFunctionSignature _ fnName [] _ = error $ "Too many arguments provided to method \"" ++ fnName ++ "\""
checkFunctionSignature _ fnName _ [] = error $ "Not enough arguments provided to method \"" ++ fnName ++ "\""

ordnum :: Int -> String
ordnum 1 = "1st"
ordnum 2 = "2nd"
ordnum 3 = "3rd"
ordnum x = (show x) ++ "th"

definitionExpr :: Metro.Expression -> Metro.Expression -> Compiler Value
definitionExpr (Metro.VariableExpr varName) ex =
  do
    varValue <- expr ex
    declareVariable varName (dataType varValue)
    return $ Value TVoid $ setLocal varName (wasmExpr varValue)
definitionExpr _ _ = error "Bad variable declaration"

assignment :: Metro.Expression -> Metro.Expression -> Compiler Value
assignment (Metro.VariableExpr i) e2 =
  do
    f2 <- expr e2
    return $ Value TVoid $ setLocal i (wasmExpr f2)
assignment (Metro.Binary Metro.Chain Metro.ThisKeyword (Metro.VariableExpr fieldName)) e2 =
  do
    classType <- requireThisContext
    fieldOffset <- getFieldOffset (show classType) fieldName
    f2 <- expr e2
    return $ Value TVoid $ i32Store (i32Add (getLocal "this") (i32Const $ toInteger fieldOffset)) (wasmExpr f2)
assignment x _ = error $ "Bad variable assignment: " ++ (show x)

binaryExprWasm :: Metro.BinOp -> Value -> Value -> Value
binaryExprWasm Metro.LogicalOr v1 v2 = boolExpr "or" v1 v2
binaryExprWasm Metro.LogicalAnd v1 v2 = boolExpr "and" v1 v2
binaryExprWasm Metro.BitwiseOr v1 v2 = arithmeticExpr "or" v1 v2
binaryExprWasm Metro.BitwiseXor v1 v2 = arithmeticExpr "xor" v1 v2
binaryExprWasm Metro.BitwiseAnd v1 v2 = arithmeticExpr "and" v1 v2
binaryExprWasm Metro.Is _e1 _e2 = falseValue -- TODO!
binaryExprWasm Metro.Unequal (Value _ e1) (Value _ e2) = Value (Primitive TBool) $ i32Eqz (i32Eq e1 e2)
binaryExprWasm Metro.Equal (Value _ e1) (Value _ e2) = Value (Primitive TBool) $ i32Eq e1 e2
binaryExprWasm Metro.Add (Value (Primitive TString) e1) (Value (Primitive TString) e2) = Value (Primitive TString) $ call "__concat" [e1, e2]
binaryExprWasm Metro.LessThan v1 v2 = comparingExpr "lt" v1 v2
binaryExprWasm Metro.LessThanOrEqual v1 v2 = comparingExpr "le" v1 v2
binaryExprWasm Metro.GreaterThan v1 v2 = comparingExpr "gt" v1 v2
binaryExprWasm Metro.GreaterThanOrEqual v1 v2 = comparingExpr "ge" v1 v2
binaryExprWasm Metro.RotateLeft v1 v2 = arithmeticExpr "rotl" v1 v2
binaryExprWasm Metro.RotateRight v1 v2 = arithmeticExpr "rotr" v1 v2
binaryExprWasm Metro.ShiftLeft v1 v2 = arithmeticExpr "shl" v1 v2
binaryExprWasm Metro.ShiftRight v1 v2 = arithmeticExpr "shr_s" v1 v2
binaryExprWasm Metro.UnsignedShiftRight v1 v2 = arithmeticExpr "shr_u" v1 v2
binaryExprWasm Metro.Subtract v1 v2 = arithmeticExpr "sub" v1 v2
binaryExprWasm Metro.Add v1 v2 = arithmeticExpr "add" v1 v2
binaryExprWasm Metro.Modulo v1 v2 = signedArithmeticExpr "rem" v1 v2
binaryExprWasm Metro.Divide v1 v2 = signedArithmeticExpr "div" v1 v2
binaryExprWasm Metro.Multiply v1 v2 = arithmeticExpr "mul" v1 v2
binaryExprWasm op _ _ = error $ (show op) ++ " not implemented yet"

--binaryExpr Metro.OptChain e1 e2 = TODO!
--binaryExpr Metro.Chain e1 e2 = TODO!

boolExpr :: String -> Value -> Value -> Value
boolExpr op (Value (Primitive TBool) e1) (Value (Primitive TBool) e2) = Value (Primitive TBool) $ WASM.Method op WASM.I32 [e1, e2]
boolExpr op _ _ = error $ "Can only apply '" ++ op ++ "' on two Bools."

comparingExpr :: String -> Value -> Value -> Value
comparingExpr op (Value (Primitive TByte) e1) (Value (Primitive TByte) e2) = Value (Primitive TBool) $ WASM.Method (op ++ "_s") WASM.I32 [e1, e2]
comparingExpr op (Value (Primitive TUByte) e1) (Value (Primitive TUByte) e2) = Value (Primitive TBool) $ WASM.Method (op ++ "_u") WASM.I32 [e1, e2]
comparingExpr op (Value (Primitive TWord) e1) (Value (Primitive TWord) e2) = Value (Primitive TBool) $ WASM.Method (op ++ "_s") WASM.I32 [e1, e2]
comparingExpr op (Value (Primitive TUWord) e1) (Value (Primitive TUWord) e2) = Value (Primitive TBool) $ WASM.Method (op ++ "_u") WASM.I32 [e1, e2]
comparingExpr op (Value (Primitive TInt) e1) (Value (Primitive TInt) e2) = Value (Primitive TBool) $ WASM.Method (op ++ "_s") WASM.I32 [e1, e2]
comparingExpr op (Value (Primitive TUInt) e1) (Value (Primitive TUInt) e2) = Value (Primitive TBool) $ WASM.Method (op ++ "_u") WASM.I32 [e1, e2]
comparingExpr op (Value (Primitive TLong) e1) (Value (Primitive TLong) e2) = Value (Primitive TBool) $ WASM.Method (op ++ "_s") WASM.I64 [e1, e2]
comparingExpr op (Value (Primitive TULong) e1) (Value (Primitive TULong) e2) = Value (Primitive TBool) $ WASM.Method (op ++ "_u") WASM.I64 [e1, e2]
comparingExpr op (Value left _) (Value right _) = error $ "Cannot apply " ++ op ++ " on " ++ (show left) ++ " and " ++ (show right) ++ "."

arithmeticExpr :: String -> Value -> Value -> Value
arithmeticExpr op (Value (Primitive TByte) e1) (Value (Primitive TByte) e2) = Value (Primitive TByte) $ toByte $ WASM.Method op WASM.I32 [e1, e2]
arithmeticExpr op (Value (Primitive TUByte) e1) (Value (Primitive TUByte) e2) = Value (Primitive TUByte) $ toByte $ WASM.Method op WASM.I32 [e1, e2]
arithmeticExpr op (Value (Primitive TWord) e1) (Value (Primitive TWord) e2) = Value (Primitive TWord) $ toWord $ WASM.Method op WASM.I32 [e1, e2]
arithmeticExpr op (Value (Primitive TUWord) e1) (Value (Primitive TUWord) e2) = Value (Primitive TUWord) $ toWord $ WASM.Method op WASM.I32 [e1, e2]
arithmeticExpr op (Value (Primitive TInt) e1) (Value (Primitive TInt) e2) = Value (Primitive TInt) $ WASM.Method op WASM.I32 [e1, e2]
arithmeticExpr op (Value (Primitive TUInt) e1) (Value (Primitive TUInt) e2) = Value (Primitive TUInt) $ WASM.Method op WASM.I32 [e1, e2]
arithmeticExpr op (Value (Primitive TLong) e1) (Value (Primitive TLong) e2) = Value (Primitive TLong) $ WASM.Method op WASM.I64 [e1, e2]
arithmeticExpr op (Value (Primitive TULong) e1) (Value (Primitive TULong) e2) = Value (Primitive TULong) $ WASM.Method op WASM.I64 [e1, e2]
arithmeticExpr op (Value left _) (Value right _) = error $ "Cannot apply " ++ op ++ " on " ++ (show left) ++ " and " ++ (show right) ++ "."

signedArithmeticExpr :: String -> Value -> Value -> Value
signedArithmeticExpr op (Value (Primitive TByte) e1) (Value (Primitive TByte) e2) = Value (Primitive TByte) $ toByte $ WASM.Method (op ++ "_s") WASM.I32 [e1, e2]
signedArithmeticExpr op (Value (Primitive TUByte) e1) (Value (Primitive TUByte) e2) = Value (Primitive TUByte) $ toByte $ WASM.Method (op ++ "_u") WASM.I32 [e1, e2]
signedArithmeticExpr op (Value (Primitive TWord) e1) (Value (Primitive TWord) e2) = Value (Primitive TWord) $ toWord $ WASM.Method (op ++ "_s") WASM.I32 [e1, e2]
signedArithmeticExpr op (Value (Primitive TUWord) e1) (Value (Primitive TUWord) e2) = Value (Primitive TUWord) $ toWord $ WASM.Method (op ++ "_u") WASM.I32 [e1, e2]
signedArithmeticExpr op (Value (Primitive TInt) e1) (Value (Primitive TInt) e2) = Value (Primitive TInt) $ WASM.Method (op ++ "_s") WASM.I32 [e1, e2]
signedArithmeticExpr op (Value (Primitive TUInt) e1) (Value (Primitive TUInt) e2) = Value (Primitive TUInt) $ WASM.Method (op ++ "_u") WASM.I32 [e1, e2]
signedArithmeticExpr op (Value (Primitive TLong) e1) (Value (Primitive TLong) e2) = Value (Primitive TLong) $ WASM.Method (op ++ "_s") WASM.I64 [e1, e2]
signedArithmeticExpr op (Value (Primitive TULong) e1) (Value (Primitive TULong) e2) = Value (Primitive TULong) $ WASM.Method (op ++ "_u") WASM.I64 [e1, e2]
signedArithmeticExpr op (Value left _) (Value right _) = error $ "Cannot apply " ++ op ++ " on " ++ (show left) ++ " and " ++ (show right) ++ "."

toByte :: WASM.Expr -> WASM.Expr
toByte e = i32And (i32Const 0xFF) e

toWord :: WASM.Expr -> WASM.Expr
toWord e = i32And (i32Const 0xFFFF) e

arguments :: Metro.Arguments -> Compiler [Value]
arguments (Metro.Args e) = exprs e
