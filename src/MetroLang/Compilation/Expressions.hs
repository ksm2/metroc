module MetroLang.Compilation.Expressions (exprs, expr, strToTypeMaybe) where

import Control.Monad.Except
import MetroLang.Compilation.Combinators
import MetroLang.Compilation.Context
import MetroLang.Compilation.Values
import MetroLang.Lang.Error
import MetroLang.Lang.Model as Metro (PrimitiveType (..), Type (..))
import qualified MetroLang.Lang.Model as Metro
import MetroLang.Location
import MetroLang.Types
import qualified MetroLang.WebAssembly.AST as WASM
import MetroLang.WebAssembly.MemoryInstr
import MetroLang.WebAssembly.Utils

exprs :: [Metro.Expression] -> Compiler [Value]
exprs = many expr

trueValue :: Value
trueValue = Value (PrimitiveType TBool) $ i32Const 1

falseValue :: Value
falseValue = Value (PrimitiveType TBool) $ i32Const 0

expr :: Metro.Expression -> Compiler Value
expr (Metro.ParenExpression e _) = expr e
expr (Metro.VarExpression varName _) =
  do
    isConst <- hasConst varName
    if isConst
      then do
        varType <- lookupConst varName
        return $ Value varType $ getGlobal varName
      else do
        refType <- strToTypeMaybe varName
        case refType of
          Just theType -> return $ Value (MetaType theType) $ i32Const 0
          Nothing -> localVarExpr varName
expr (Metro.LiteralExpression lit _) = literal lit
expr (Metro.NullExpression _) = return $ Value VoidType $ i32Const 0
expr (Metro.ThisExpression _) =
  do
    classType <- requireThisContext
    return $ Value classType $ getLocal "this"
expr e@(Metro.UnaryExpression op e1 _) = unaryExpr e op e1
expr e@(Metro.BinaryExpression op e1 e2 _) = binaryExpr e op e1 e2
expr (Metro.CastExpression e1 typ _) = castExpr e1 typ
expr (Metro.CallExpression callee args _) =
  do
    a <- arguments args
    isConstructorCall <- classExists callee
    if isConstructorCall
      then return $ Value (RefType callee) $ call callee (map wasmExpr a)
      else functionCall callee a
expr (Metro.IndexExpression obj key _) = listAccessExpr obj key
expr (Metro.MatchExpression t body _) = matchExpr t body
expr (Metro.MethodCallExpression obj methodName args _) =
  do
    objValue <- expr obj
    argValues <- arguments args
    methodCall objValue methodName argValues
expr e@(Metro.AccessExpression obj fieldName _) =
  do
    objValue <- expr obj
    fieldAccess e objValue fieldName
expr (Metro.TypeExpression t _) = return $ Value (MetaType t) $ i32Const 0 -- TODO: Use meaningful const

literal :: Metro.Literal -> Compiler Value
literal (Metro.BoolLiteral True _) = return trueValue
literal (Metro.BoolLiteral False _) = return falseValue
literal (Metro.IntLiteral n _) = return $ Value (PrimitiveType TInt) $ i32Const $ toInteger n
literal (Metro.UIntLiteral n _) = return $ Value (PrimitiveType TUInt) $ i32Const $ toInteger n
literal (Metro.ByteLiteral n _) = return $ Value (PrimitiveType TByte) $ i32Const $ toInteger n
literal (Metro.StringLiteral l _) =
  do
    ptr <- registerString l
    return $ Value (PrimitiveType TString) $ i32Const (toInteger ptr)

functionCall :: String -> [Value] -> Compiler Value
functionCall fnName args =
  do
    functionInfo <- lookupFunction fnName
    let wasmArgs = checkFunctionSignature 1 fnName (parameters functionInfo) args
    return $ Value (returnDataType functionInfo) $ call fnName wasmArgs

castExpr :: Metro.Expression -> Metro.Type -> Compiler Value
castExpr e castType =
  do
    Value _ f <- expr e
    return $ Value castType f

strToTypeMaybe :: String -> Compiler (Maybe Type)
strToTypeMaybe str =
  do
    let isPrimitive = strToPrimitiveType str
    isClass <- strToClass str
    return $ isPrimitive ?? isClass

(??) :: Maybe a -> Maybe a -> Maybe a
(Just x) ?? _ = Just x
Nothing ?? f = f

strToPrimitiveType :: String -> Maybe Type
strToPrimitiveType "Bool" = Just $ PrimitiveType TBool
strToPrimitiveType "IntXS" = Just $ PrimitiveType TIntXS
strToPrimitiveType "Byte" = Just $ PrimitiveType TByte
strToPrimitiveType "IntS" = Just $ PrimitiveType TIntS
strToPrimitiveType "Word" = Just $ PrimitiveType TWord
strToPrimitiveType "Int" = Just $ PrimitiveType TInt
strToPrimitiveType "UInt" = Just $ PrimitiveType TUInt
strToPrimitiveType "IntL" = Just $ PrimitiveType TIntL
strToPrimitiveType "UIntL" = Just $ PrimitiveType TUIntL
strToPrimitiveType "Float" = Just $ PrimitiveType TFloat
strToPrimitiveType "FloatL" = Just $ PrimitiveType TFloatL
strToPrimitiveType "Char" = Just $ PrimitiveType TChar
strToPrimitiveType "String" = Just $ PrimitiveType TString
strToPrimitiveType _ = Nothing

strToClass :: String -> Compiler (Maybe Type)
strToClass className =
  do
    isClass <- classExists className
    return $
      if isClass
        then Just $ RefType className
        else Nothing

localVarExpr :: String -> Compiler Value
localVarExpr varName =
  do
    varType <- lookupVariableType varName
    return $ Value varType $ getLocal varName

unaryExpr :: Metro.Expression -> Metro.UnaryOperator -> Metro.Expression -> Compiler Value
unaryExpr e op e1 = expr e1 >>= unaryExprWasm e op

unaryExprWasm :: Metro.Expression -> Metro.UnaryOperator -> Value -> Compiler Value
unaryExprWasm e Metro.Neg v@(Value t _) = case t of
  PrimitiveType TInt -> return $ vmap (i32Sub (i32Const 0)) v
  _ -> throwCompilationError e $ "Cannot negate value of type " ++ show t
unaryExprWasm e Metro.LogicalNot v@(Value t _) = case t of
  PrimitiveType TBool -> return $ vmap i32Eqz v
  _ -> throwCompilationError e $ "Cannot apply 'not' on value of type " ++ show t
unaryExprWasm e Metro.BitwiseNot v@(Value (PrimitiveType p) _)
  | p == TByte || p == TIntXS = return $ vmap (i32Xor (i32Const 0xFF)) v
  | p == TWord || p == TIntS = return $ vmap (i32Xor (i32Const 0xFFFF)) v
  | p == TUInt || p == TInt = return $ vmap (i32Xor (i32Const 0xFFFFFFFF)) v
  | p == TUIntL || p == TIntL = return $ vmap (i64Xor (i64Const 0xFFFFFFFFFFFFFFFF)) v
unaryExprWasm e Metro.BitwiseNot (Value t _) = throwCompilationError e $ "Cannot apply '~' on value of type " ++ show t

binaryExpr :: Metro.Expression -> Metro.BinaryOperator -> Metro.Expression -> Metro.Expression -> Compiler Value
binaryExpr e op e1 e2 = case op of
  Metro.Assignment -> assignment e1 e2
  Metro.AssignBitwiseOr -> assignmentOp e Metro.BitwiseOr e1 e2
  Metro.AssignBitwiseXor -> assignmentOp e Metro.BitwiseXor e1 e2
  Metro.AssignBitwiseAnd -> assignmentOp e Metro.BitwiseAnd e1 e2
  Metro.AssignRotateLeft -> assignmentOp e Metro.RotateLeft e1 e2
  Metro.AssignRotateRight -> assignmentOp e Metro.RotateRight e1 e2
  Metro.AssignShiftLeft -> assignmentOp e Metro.ShiftLeft e1 e2
  Metro.AssignShiftRight -> assignmentOp e Metro.ShiftRight e1 e2
  Metro.AssignSubtract -> assignmentOp e Metro.Subtract e1 e2
  Metro.AssignAdd -> assignmentOp e Metro.Add e1 e2
  Metro.AssignModulo -> assignmentOp e Metro.Modulo e1 e2
  Metro.AssignDivide -> assignmentOp e Metro.Divide e1 e2
  Metro.AssignMultiply -> assignmentOp e Metro.Multiply e1 e2
  _ -> do
    f1 <- expr e1
    f2 <- expr e2
    binaryExprWasm e op f1 f2

fieldAccess :: Metro.Expression -> Value -> String -> Compiler Value
fieldAccess e (Value (MetaType (PrimitiveType TInt)) _) "MIN_VALUE" = return $ Value (PrimitiveType TInt) $ i32Const $ negate 2147483648
fieldAccess e (Value (MetaType (PrimitiveType TInt)) _) "MAX_VALUE" = return $ Value (PrimitiveType TInt) $ i32Const 2147483647
fieldAccess e (Value (PrimitiveType TUInt) obj) "lowWord" = return $ convertTo TUInt TWord obj
fieldAccess e (Value (PrimitiveType TUInt) obj) "highWord" = return $ Value (PrimitiveType TWord) $ i32Shru obj $ i32Const 16
fieldAccess e (Value (PrimitiveType TString) obj) "length" = return $ Value (PrimitiveType TInt) $ loadInstr TInt 0 obj
fieldAccess e (Value (ArrayType _) obj) "length" = return $ Value (PrimitiveType TUInt) $ loadInstr TUInt 0 obj
fieldAccess e (Value (GenericType g _) objExpr) fieldName = fieldAccess e (Value g objExpr) fieldName
fieldAccess e (Value (RefType className) objExpr) fieldName =
  do
    fieldOffset <- getFieldOffset className fieldName
    return $ Value (PrimitiveType TInt) $ loadInstr TInt fieldOffset objExpr
-- TODO: can only load Int
fieldAccess e (Value objType _) methodName = throwCompilationError e $ "Unknown field " ++ methodName ++ " on primitive type " ++ show objType

methodCall :: Value -> String -> [Value] -> Compiler Value
methodCall (Value (PrimitiveType p) obj) "toIntXS" [] = return $ convertTo p TIntXS obj
methodCall (Value (PrimitiveType p) obj) "toIntS" [] = return $ convertTo p TIntS obj
methodCall (Value (PrimitiveType p) obj) "toInt" [] = return $ convertTo p TInt obj
methodCall (Value (PrimitiveType p) obj) "toIntL" [] = return $ convertTo p TIntL obj
methodCall (Value (PrimitiveType p) obj) "toByte" [] = return $ convertTo p TByte obj
methodCall (Value (PrimitiveType p) obj) "toWord" [] = return $ convertTo p TWord obj
methodCall (Value (PrimitiveType p) obj) "toUInt" [] = return $ convertTo p TUInt obj
methodCall (Value (PrimitiveType p) obj) "toUIntL" [] = return $ convertTo p TUIntL obj
methodCall (Value (PrimitiveType p) obj) "store" [arg0] = return $ Value VoidType $ storeInstr p 0 (wasmExpr arg0) obj
methodCall (Value (PrimitiveType p) obj) "countLeadingZeros" [] = return $ clz p obj
methodCall (Value (PrimitiveType p) obj) "countTrailingZeros" [] = return $ ctz p obj
methodCall (Value (PrimitiveType p) obj) "countOnes" [] = return $ popcnt p obj
methodCall (Value (PrimitiveType TString) obj) "toByteList" [] = return $ Value (ArrayType (PrimitiveType TByte)) obj
methodCall (Value (MetaType typeRef) _) methodName args = staticClassMethodCall typeRef methodName args
methodCall (Value objType obj) methodName args = classMethodCall (show objType) obj methodName args

--methodCall (Value objType _) methodName args = error $ "Unknown method " ++ methodName ++ argsToInfo args ++ " on primitive type " ++ show objType

staticClassMethodCall :: Type -> String -> [Value] -> Compiler Value
staticClassMethodCall (PrimitiveType p) "load" [arg0] = return $ Value (PrimitiveType p) $ loadInstr p 0 (wasmExpr arg0)
staticClassMethodCall (PrimitiveType p) "store" [arg0, arg1] = return $ Value VoidType $ storeInstr p 0 (wasmExpr arg1) (wasmExpr arg0)
staticClassMethodCall typeRef methodName args =
  do
    method <- lookupClassMethod (show typeRef) methodName
    let wasmArgs = checkFunctionSignature 1 methodName (parameters method) args
    return $ Value (returnDataType method) $ call (show typeRef ++ "." ++ methodName) wasmArgs

classMethodCall :: String -> WASM.Expr -> String -> [Value] -> Compiler Value
classMethodCall className obj methodName args =
  do
    method <- lookupClassMethod className methodName
    let wasmArgs = checkFunctionSignature 1 methodName (parameters method) args
    return $ Value (returnDataType method) $ call (className ++ "." ++ methodName) $ obj : wasmArgs

clz :: Metro.PrimitiveType -> WASM.Expr -> Value
clz p e
  | p == TByte || p == TIntXS = Value (PrimitiveType TByte) $ WASM.Method "clz" WASM.I32 [i32Shl (i32Const 24) e]
  | p == TWord || p == TIntS = Value (PrimitiveType TByte) $ WASM.Method "clz" WASM.I32 [i32Shl (i32Const 16) e]
  | p == TUInt || p == TInt = Value (PrimitiveType TByte) $ WASM.Method "clz" WASM.I32 [e]
  | p == TUIntL || p == TIntL = Value (PrimitiveType TByte) $ WASM.Method "clz" WASM.I64 [e]
  | otherwise = error $ "Cannot count leading zeros for " ++ show p

ctz :: Metro.PrimitiveType -> WASM.Expr -> Value
ctz p e
  | p <= TUInt || p <= TInt = Value (PrimitiveType TByte) $ WASM.Method "ctz" WASM.I32 [e]
  | p == TUIntL || p == TIntL = Value (PrimitiveType TByte) $ WASM.Method "ctz" WASM.I64 [e]
  | otherwise = error $ "Cannot count trailing zeros for " ++ show p

popcnt :: Metro.PrimitiveType -> WASM.Expr -> Value
popcnt p e
  | p <= TUInt || p <= TInt = Value (PrimitiveType TByte) $ WASM.Method "popcnt" WASM.I32 [e]
  | p == TUIntL || p == TIntL = Value (PrimitiveType TByte) $ WASM.Method "popcnt" WASM.I64 [e]
  | otherwise = error $ "Cannot count ones for " ++ show p

listAccessExpr :: Metro.Expression -> Metro.Expression -> Compiler Value
listAccessExpr obj key =
  do
    Value keyType keyExpr <- expr key
    if keyType /= PrimitiveType TUInt
      then throwCompilationError key "List access needs to be of type UInt"
      else do
        Value objType objExpr <- expr obj
        case objType of
          ArrayType listType -> return $ load listType $ i32Add (i32Const 4) $ i32Add objExpr $ i32Mul keyExpr $ i32Const $ toInteger (sizeOf listType)
          _ -> throwCompilationError obj "Only lists can be accessed by index"

matchExpr :: Metro.Expression -> Metro.MatchBody -> Compiler Value
matchExpr target (Metro.MatchBody rules _) = matchRules rules target

matchRules :: [Metro.MatchRule] -> Metro.Expression -> Compiler Value
matchRules [] e = throwCompilationError e "There must be at least one rule in a match block"
matchRules [Metro.MatchRule (Metro.MatchWildcard _) caseVal _] _ =
  do
    Value valType valExpr <- expr caseVal
    return $ Value valType valExpr
matchRules [_] _ = error "Invalid last condition, it must be a wildcard."
matchRules (c : cs) target =
  do
    let Metro.MatchRule caseCond caseVal _ = c
    Value valType valExpr <- expr caseVal
    Value elseType elseExpr <- matchRules cs target
    if elseType /= valType
      then error "Return types of all match rules must be consistent."
      else do
        cond <- makeMatchRuleCond target caseCond
        return $ Value valType $ WASM.Select valExpr elseExpr cond

makeMatchRuleCond :: Metro.Expression -> Metro.MatchCondition -> Compiler WASM.Expr
makeMatchRuleCond left (Metro.MatchWildcard _) = wasmExpr <$> expr left
makeMatchRuleCond left (Metro.MatchPattern lit _) =
  do
    Value leftType leftExpr <- expr left
    Value rightType rightExpr <- literal lit
    if leftType /= rightType
      then error $ "Match rule condition type " ++ show rightType ++ " does not match type of value to match " ++ show leftType
      else return $ i32Eq leftExpr rightExpr

load :: Type -> WASM.Expr -> Value
load (PrimitiveType p) n1 = Value (PrimitiveType p) $ loadInstr p 0 n1
load x n1 = Value x $ WASM.Method "load" WASM.I32 [n1]

checkFunctionSignature :: Int -> String -> [Metro.Type] -> [Value] -> [WASM.Expr]
checkFunctionSignature _ _ [] [] = []
checkFunctionSignature no fnName (p : params) (a : args) =
  let Value dt ex = a
   in if p == dt
        then ex : checkFunctionSignature (no + 1) fnName params args
        else error $ "The " ++ ordnum no ++ " parameter type of \"" ++ fnName ++ "\" does not match: Expected " ++ show p ++ ", but was " ++ show dt
checkFunctionSignature _ fnName [] _ = error $ "Too many arguments provided to method \"" ++ fnName ++ "\""
checkFunctionSignature _ fnName _ [] = error $ "Not enough arguments provided to method \"" ++ fnName ++ "\""

ordnum :: Int -> String
ordnum 1 = "1st"
ordnum 2 = "2nd"
ordnum 3 = "3rd"
ordnum x = show x ++ "th"

assignment :: Metro.Expression -> Metro.Expression -> Compiler Value
assignment left right =
  do
    parsedLeft <- leftHandSideSetter left
    parsedRight <- expr right
    return $ Value VoidType $ parsedLeft (wasmExpr parsedRight)

assignmentOp :: Metro.Expression -> Metro.BinaryOperator -> Metro.Expression -> Metro.Expression -> Compiler Value
assignmentOp e op left right =
  do
    setterLeft <- leftHandSideSetter left
    getterLeft <- leftHandSideGetter left
    parsedRight <- expr right
    resultValue <- binaryExprWasm e op getterLeft parsedRight
    return $ Value VoidType $ setterLeft $ wasmExpr resultValue

leftHandSideGetter :: Metro.Expression -> Compiler Value
leftHandSideGetter (Metro.VarExpression varName _) = localVarExpr varName
leftHandSideGetter (Metro.AccessExpression (Metro.ThisExpression _) fieldName _) =
  do
    classType <- requireThisContext
    fieldOffset <- getFieldOffset (show classType) fieldName
    return $ Value (PrimitiveType TInt) $ loadInstr TInt fieldOffset (getLocal "this")
leftHandSideGetter e = throwCompilationError e "Not a valid left-hand side assignment expression"

leftHandSideSetter :: Metro.Expression -> Compiler (WASM.Expr -> WASM.Expr)
leftHandSideSetter (Metro.VarExpression i _) = return $ setLocal i
leftHandSideSetter (Metro.AccessExpression (Metro.ThisExpression _) fieldName _) =
  do
    classType <- requireThisContext
    fieldOffset <- getFieldOffset (show classType) fieldName
    return $ storeInstr TInt fieldOffset (getLocal "this")
leftHandSideSetter e = throwCompilationError e "Not a valid left-hand side assignment expression"

binaryExprWasm :: Metro.Expression -> Metro.BinaryOperator -> Value -> Value -> Compiler Value
binaryExprWasm e op v1 v2 = case op of
  Metro.LogicalOr -> boolExpr e "or" v1 v2
  Metro.LogicalAnd -> boolExpr e "and" v1 v2
  Metro.BitwiseOr -> arithmeticExpr e "or" v1 v2
  Metro.BitwiseXor -> arithmeticExpr e "xor" v1 v2
  Metro.BitwiseAnd -> arithmeticExpr e "and" v1 v2
  Metro.Is -> return falseValue -- TODO!
  Metro.Unequal -> return $ Value (PrimitiveType TBool) $ i32Eqz (i32Eq (wasmExpr v1) (wasmExpr v2))
  Metro.Equal -> return $ Value (PrimitiveType TBool) $ i32Eq (wasmExpr v1) (wasmExpr v2)
  Metro.LessThan -> signedComparingExpr e "lt" v1 v2
  Metro.LessThanOrEqual -> signedComparingExpr e "le" v1 v2
  Metro.GreaterThan -> signedComparingExpr e "gt" v1 v2
  Metro.GreaterThanOrEqual -> signedComparingExpr e "ge" v1 v2
  Metro.RotateLeft -> arithmeticExpr e "rotl" v1 v2
  Metro.RotateRight -> arithmeticExpr e "rotr" v1 v2
  Metro.ShiftLeft -> arithmeticExpr e "shl" v1 v2
  Metro.ShiftRight -> signedArithmeticExpr e "shr" v1 v2
  Metro.Subtract -> arithmeticExpr e "sub" v1 v2
  Metro.Add | dataType v1 == PrimitiveType TString && dataType v2 == PrimitiveType TString -> return $ Value (PrimitiveType TString) $ call "__concat" [wasmExpr v1, wasmExpr v2]
  Metro.Add -> arithmeticExpr e "add" v1 v2
  Metro.Modulo -> signedArithmeticExpr e "rem" v1 v2
  Metro.Divide -> signedArithmeticExpr e "div" v1 v2
  Metro.Multiply -> arithmeticExpr e "mul" v1 v2
  _ -> throwCompilationError e $ show op ++ " not implemented yet"

--binaryExpr Metro.OptChain e1 e2 = TODO!
--binaryExpr Metro.Chain e1 e2 = TODO!

boolExpr :: Metro.Expression -> String -> Value -> Value -> Compiler Value
boolExpr _ op (Value (PrimitiveType TBool) e1) (Value (PrimitiveType TBool) e2) = return $ Value (PrimitiveType TBool) $ WASM.Method op WASM.I32 [e1, e2]
boolExpr e op v1 e2 = throwCompilationError e $ "Cannot apply '" ++ op ++ "' on " ++ vtype v1 ++ " and " ++ vtype e2

signedComparingExpr :: Metro.Expression -> String -> Value -> Value -> Compiler Value
signedComparingExpr e op (Value (PrimitiveType left) e1) (Value (PrimitiveType right) e2)
  | isSignedType left && isSignedType right = comparingExpr e (op ++ "_s") (Value (PrimitiveType left) e1) (Value (PrimitiveType right) e2)
  | isUnsignedType left && isUnsignedType right = comparingExpr e (op ++ "_u") (Value (PrimitiveType left) e1) (Value (PrimitiveType right) e2)
signedComparingExpr e op v1 v2 = throwCompilationError e $ "Cannot compare " ++ vtype v1 ++ " and " ++ vtype v2

comparingExpr :: Metro.Expression -> String -> Value -> Value -> Compiler Value
comparingExpr e op (Value (PrimitiveType left) e1) (Value (PrimitiveType right) e2)
  | left == right = return $ Value (PrimitiveType TBool) $ WASM.Method op (dataTypeToValtype (PrimitiveType right)) [e1, e2]
  | left < right = return $ Value (PrimitiveType TBool) $ WASM.Method op (dataTypeToValtype (PrimitiveType right)) [convertToExpr left right e1, e2]
  | left > right = return $ Value (PrimitiveType TBool) $ WASM.Method op (dataTypeToValtype (PrimitiveType left)) [e1, convertToExpr right left e2]
comparingExpr e op v1 v2 = throwCompilationError e $ "Cannot apply " ++ op ++ " on " ++ vtype v1 ++ " and " ++ vtype v2

arithmeticExpr :: Metro.Expression -> String -> Value -> Value -> Compiler Value
arithmeticExpr e op (Value (PrimitiveType left) e1) (Value (PrimitiveType right) e2)
  | left == right = return $ maskTo right $ WASM.Method op (dataTypeToValtype (PrimitiveType right)) [e1, e2]
  | left < right = return $ maskTo right $ WASM.Method op (dataTypeToValtype (PrimitiveType right)) [convertToExpr left right e1, e2]
  | left > right = return $ maskTo left $ WASM.Method op (dataTypeToValtype (PrimitiveType left)) [e1, convertToExpr right left e2]
arithmeticExpr e op v1 v2 = throwCompilationError e $ "Cannot apply " ++ op ++ " on " ++ vtype v1 ++ " and " ++ vtype v2

signedArithmeticExpr :: Metro.Expression -> String -> Value -> Value -> Compiler Value
signedArithmeticExpr e op v1@(Value (PrimitiveType left) e1) v2@(Value (PrimitiveType right) e2)
  | isSignedType left && isSignedType right = arithmeticExpr e (op ++ "_s") v1 v2
  | isUnsignedType left && isUnsignedType right = arithmeticExpr e (op ++ "_u") v1 v2
signedArithmeticExpr e _ _ _ = throwCompilationError e "Cannot perform arithmetics on mixed signed/unsigned type"

maskTo :: Metro.PrimitiveType -> WASM.Expr -> Value
maskTo dest = Value (PrimitiveType dest) . maskToExpr dest

maskToExpr :: Metro.PrimitiveType -> WASM.Expr -> WASM.Expr
maskToExpr TByte = i32And (i32Const 0xFF)
maskToExpr TIntXS = i32And (i32Const 0xFF)
maskToExpr TWord = i32And (i32Const 0xFFFF)
maskToExpr TIntS = i32And (i32Const 0xFFFF)
maskToExpr _ = id

convertTo :: Metro.PrimitiveType -> Metro.PrimitiveType -> WASM.Expr -> Value
convertTo src dest = Value (PrimitiveType dest) . convertToExpr src dest

convertToExpr :: Metro.PrimitiveType -> Metro.PrimitiveType -> WASM.Expr -> WASM.Expr
convertToExpr src TUIntL
  | src <= TInt = i64ExtendI32U
  | src <= TUInt = i64ExtendI32U
  | otherwise = error $ "Cannot convert " ++ show src ++ " to UIntL"
convertToExpr src TIntL
  | src <= TInt = i64ExtendI32S
  | src <= TUInt = i64ExtendI32S
  | otherwise = error $ "Cannot convert " ++ show src ++ " to IntL"
convertToExpr src dest
  | src == dest = id
  | src < dest = id
  | src > dest = maskToExpr dest
  | src == unsigned dest = id
  | otherwise = error $ "Cannot convert " ++ show src ++ " to " ++ show dest

arguments :: Metro.Arguments -> Compiler [Value]
arguments (Metro.Arguments e _) = exprs e

throwCompilationError :: Locatable a => a -> String -> Compiler b
throwCompilationError l msg = throwError $ CompilationError (loc l) msg
