module MetroLang.Compilation.Expressions (exprs, expr, strToTypeMaybe) where

import MetroLang.Compilation.Combinators
import MetroLang.Compilation.Context
import MetroLang.Compilation.Values
import MetroLang.Lang.Model as Metro (PrimitiveType (..), Type (..))
import qualified MetroLang.Lang.Model as Metro
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
expr (Metro.ParenExpression e) = expr e
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
expr (Metro.UnaryExpression op e _) = unaryExpr op e
expr (Metro.BinaryExpression op e1 e2 _) = binaryExpr op e1 e2
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
expr (Metro.AccessExpression obj fieldName _) =
  do
    objValue <- expr obj
    fieldAccess objValue fieldName
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

strToType :: String -> Compiler Type
strToType str =
  do
    maybeType <- strToTypeMaybe str
    case maybeType of
      Just x -> return x
      Nothing -> error $ "Not a valid type reference: " ++ str

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

unaryExpr :: Metro.UnaryOperator -> Metro.Expression -> Compiler Value
unaryExpr op e = expr e >>= \value -> return $ unaryExprWasm op value

unaryExprWasm :: Metro.UnaryOperator -> Value -> Value
unaryExprWasm Metro.Neg (Value (PrimitiveType TInt) e) = Value (PrimitiveType TInt) $ i32Sub (i32Const 0) e
unaryExprWasm Metro.Neg _ = error "Cannot negate the given value."
unaryExprWasm Metro.LogicalNot (Value (PrimitiveType TBool) e) = Value (PrimitiveType TBool) $ i32Eqz e
unaryExprWasm Metro.LogicalNot _ = error "Can only apply 'not' on a Bool."
unaryExprWasm Metro.BitwiseNot (Value (PrimitiveType p) e)
  | p == TByte || p == TIntXS = Value (PrimitiveType p) $ i32Xor (i32Const 0xFF) e
  | p == TWord || p == TIntS = Value (PrimitiveType p) $ i32Xor (i32Const 0xFFFF) e
  | p == TUInt || p == TInt = Value (PrimitiveType p) $ i32Xor (i32Const 0xFFFFFFFF) e
  | p == TUIntL || p == TIntL = Value (PrimitiveType p) $ i64Xor (i64Const 0xFFFFFFFFFFFFFFFF) e
unaryExprWasm Metro.BitwiseNot (Value t _) = error $ "Cannot apply '~' on " ++ show t

binaryExpr :: Metro.BinaryOperator -> Metro.Expression -> Metro.Expression -> Compiler Value
binaryExpr Metro.Assignment e1 e2 = assignment e1 e2
binaryExpr Metro.AssignBitwiseOr e1 e2 = assignmentOp Metro.BitwiseOr e1 e2
binaryExpr Metro.AssignBitwiseXor e1 e2 = assignmentOp Metro.BitwiseXor e1 e2
binaryExpr Metro.AssignBitwiseAnd e1 e2 = assignmentOp Metro.BitwiseAnd e1 e2
binaryExpr Metro.AssignRotateLeft e1 e2 = assignmentOp Metro.RotateLeft e1 e2
binaryExpr Metro.AssignRotateRight e1 e2 = assignmentOp Metro.RotateRight e1 e2
binaryExpr Metro.AssignShiftLeft e1 e2 = assignmentOp Metro.ShiftLeft e1 e2
binaryExpr Metro.AssignShiftRight e1 e2 = assignmentOp Metro.ShiftRight e1 e2
binaryExpr Metro.AssignSubtract e1 e2 = assignmentOp Metro.Subtract e1 e2
binaryExpr Metro.AssignAdd e1 e2 = assignmentOp Metro.Add e1 e2
binaryExpr Metro.AssignModulo e1 e2 = assignmentOp Metro.Modulo e1 e2
binaryExpr Metro.AssignDivide e1 e2 = assignmentOp Metro.Divide e1 e2
binaryExpr Metro.AssignMultiply e1 e2 = assignmentOp Metro.Multiply e1 e2
binaryExpr op e1 e2 =
  do
    f1 <- expr e1
    f2 <- expr e2
    return $ binaryExprWasm op f1 f2

fieldAccess :: Value -> String -> Compiler Value
fieldAccess (Value (MetaType (PrimitiveType TInt)) _) "MIN_VALUE" = return $ Value (PrimitiveType TInt) $ i32Const $ negate 2147483648
fieldAccess (Value (MetaType (PrimitiveType TInt)) _) "MAX_VALUE" = return $ Value (PrimitiveType TInt) $ i32Const 2147483647
fieldAccess (Value (PrimitiveType TUInt) obj) "lowWord" = return $ convertTo TUInt TWord obj
fieldAccess (Value (PrimitiveType TUInt) obj) "highWord" = return $ Value (PrimitiveType TWord) $ i32Shru obj $ i32Const 16
fieldAccess (Value (PrimitiveType TString) obj) "length" = return $ Value (PrimitiveType TInt) $ loadInstr TInt 0 obj
fieldAccess (Value (ArrayType _) obj) "length" = return $ Value (PrimitiveType TUInt) $ loadInstr TUInt 0 obj
fieldAccess (Value (GenericType g _) objExpr) fieldName = fieldAccess (Value g objExpr) fieldName
fieldAccess (Value (RefType className) objExpr) fieldName =
  do
    fieldOffset <- getFieldOffset className fieldName
    return $ Value (PrimitiveType TInt) $ loadInstr TInt fieldOffset objExpr
-- TODO: can only load Int
fieldAccess (Value objType _) methodName = error $ "Unknown field " ++ methodName ++ " on primitive type " ++ show objType

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
      then error "List access needs to be of type UInt."
      else do
        Value objType objExpr <- expr obj
        case objType of
          ArrayType listType -> return $ load listType $ i32Add (i32Const 4) $ i32Add objExpr $ i32Mul keyExpr $ i32Const $ toInteger (sizeOf listType)
          _ -> error "Can only access lists by index."

matchExpr :: Metro.Expression -> Metro.MatchBody -> Compiler Value
matchExpr target (Metro.MatchBody rules _) = matchRules rules target

matchRules :: [Metro.MatchRule] -> Metro.Expression -> Compiler Value
matchRules [] _ = error "There must be at least one rule in a match block."
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

assignmentOp :: Metro.BinaryOperator -> Metro.Expression -> Metro.Expression -> Compiler Value
assignmentOp op left right =
  do
    setterLeft <- leftHandSideSetter left
    getterLeft <- leftHandSideGetter left
    parsedRight <- expr right
    return $ Value VoidType $ setterLeft $ wasmExpr $ binaryExprWasm op getterLeft parsedRight

leftHandSideGetter :: Metro.Expression -> Compiler Value
leftHandSideGetter (Metro.VarExpression varName _) = localVarExpr varName
leftHandSideGetter (Metro.AccessExpression (Metro.ThisExpression _) fieldName _) =
  do
    classType <- requireThisContext
    fieldOffset <- getFieldOffset (show classType) fieldName
    return $ Value (PrimitiveType TInt) $ loadInstr TInt fieldOffset (getLocal "this")
leftHandSideGetter _ = error "Not a valid left-hand side assignment expression."

leftHandSideSetter :: Metro.Expression -> Compiler (WASM.Expr -> WASM.Expr)
leftHandSideSetter (Metro.VarExpression i _) = return $ setLocal i
leftHandSideSetter (Metro.AccessExpression (Metro.ThisExpression _) fieldName _) =
  do
    classType <- requireThisContext
    fieldOffset <- getFieldOffset (show classType) fieldName
    return $ storeInstr TInt fieldOffset (getLocal "this")
leftHandSideSetter _ = error "Not a valid left-hand side assignment expression."

binaryExprWasm :: Metro.BinaryOperator -> Value -> Value -> Value
binaryExprWasm Metro.LogicalOr v1 v2 = boolExpr "or" v1 v2
binaryExprWasm Metro.LogicalAnd v1 v2 = boolExpr "and" v1 v2
binaryExprWasm Metro.BitwiseOr v1 v2 = arithmeticExpr "or" v1 v2
binaryExprWasm Metro.BitwiseXor v1 v2 = arithmeticExpr "xor" v1 v2
binaryExprWasm Metro.BitwiseAnd v1 v2 = arithmeticExpr "and" v1 v2
binaryExprWasm Metro.Is _e1 _e2 = falseValue -- TODO!
binaryExprWasm Metro.Unequal (Value _ e1) (Value _ e2) = Value (PrimitiveType TBool) $ i32Eqz (i32Eq e1 e2)
binaryExprWasm Metro.Equal (Value _ e1) (Value _ e2) = Value (PrimitiveType TBool) $ i32Eq e1 e2
binaryExprWasm Metro.Add (Value (PrimitiveType TString) e1) (Value (PrimitiveType TString) e2) = Value (PrimitiveType TString) $ call "__concat" [e1, e2]
binaryExprWasm Metro.LessThan v1 v2 = signedComparingExpr "lt" v1 v2
binaryExprWasm Metro.LessThanOrEqual v1 v2 = signedComparingExpr "le" v1 v2
binaryExprWasm Metro.GreaterThan v1 v2 = signedComparingExpr "gt" v1 v2
binaryExprWasm Metro.GreaterThanOrEqual v1 v2 = signedComparingExpr "ge" v1 v2
binaryExprWasm Metro.RotateLeft v1 v2 = arithmeticExpr "rotl" v1 v2
binaryExprWasm Metro.RotateRight v1 v2 = arithmeticExpr "rotr" v1 v2
binaryExprWasm Metro.ShiftLeft v1 v2 = arithmeticExpr "shl" v1 v2
binaryExprWasm Metro.ShiftRight v1 v2 = signedArithmeticExpr "shr" v1 v2
binaryExprWasm Metro.Subtract v1 v2 = arithmeticExpr "sub" v1 v2
binaryExprWasm Metro.Add v1 v2 = arithmeticExpr "add" v1 v2
binaryExprWasm Metro.Modulo v1 v2 = signedArithmeticExpr "rem" v1 v2
binaryExprWasm Metro.Divide v1 v2 = signedArithmeticExpr "div" v1 v2
binaryExprWasm Metro.Multiply v1 v2 = arithmeticExpr "mul" v1 v2
binaryExprWasm op _ _ = error $ show op ++ " not implemented yet"

--binaryExpr Metro.OptChain e1 e2 = TODO!
--binaryExpr Metro.Chain e1 e2 = TODO!

boolExpr :: String -> Value -> Value -> Value
boolExpr op (Value (PrimitiveType TBool) e1) (Value (PrimitiveType TBool) e2) = Value (PrimitiveType TBool) $ WASM.Method op WASM.I32 [e1, e2]
boolExpr op _ _ = error $ "Can only apply '" ++ op ++ "' on two Bools."

signedComparingExpr :: String -> Value -> Value -> Value
signedComparingExpr op (Value (PrimitiveType left) e1) (Value (PrimitiveType right) e2)
  | isSignedType left && isSignedType right = comparingExpr (op ++ "_s") (Value (PrimitiveType left) e1) (Value (PrimitiveType right) e2)
  | isUnsignedType left && isUnsignedType right = comparingExpr (op ++ "_u") (Value (PrimitiveType left) e1) (Value (PrimitiveType right) e2)
signedComparingExpr op x y = error "Cannot perform comparison on mixed signed/unsigned type"

comparingExpr :: String -> Value -> Value -> Value
comparingExpr op (Value (PrimitiveType left) e1) (Value (PrimitiveType right) e2)
  | left == right = Value (PrimitiveType TBool) $ WASM.Method op (dataTypeToValtype (PrimitiveType right)) [e1, e2]
  | left < right = Value (PrimitiveType TBool) $ WASM.Method op (dataTypeToValtype (PrimitiveType right)) [convertToExpr left right e1, e2]
  | left > right = Value (PrimitiveType TBool) $ WASM.Method op (dataTypeToValtype (PrimitiveType left)) [e1, convertToExpr right left e2]
comparingExpr op (Value left _) (Value right _) = error $ "Cannot apply " ++ op ++ " on " ++ show left ++ " and " ++ show right ++ "."

arithmeticExpr :: String -> Value -> Value -> Value
arithmeticExpr op (Value (PrimitiveType left) e1) (Value (PrimitiveType right) e2)
  | left == right = maskTo right $ WASM.Method op (dataTypeToValtype (PrimitiveType right)) [e1, e2]
  | left < right = maskTo right $ WASM.Method op (dataTypeToValtype (PrimitiveType right)) [convertToExpr left right e1, e2]
  | left > right = maskTo left $ WASM.Method op (dataTypeToValtype (PrimitiveType left)) [e1, convertToExpr right left e2]
arithmeticExpr op (Value left _) (Value right _) = error $ "Cannot apply " ++ op ++ " on " ++ show left ++ " and " ++ show right ++ "."

signedArithmeticExpr :: String -> Value -> Value -> Value
signedArithmeticExpr op (Value (PrimitiveType left) e1) (Value (PrimitiveType right) e2)
  | isSignedType left && isSignedType right = arithmeticExpr (op ++ "_s") (Value (PrimitiveType left) e1) (Value (PrimitiveType right) e2)
  | isUnsignedType left && isUnsignedType right = arithmeticExpr (op ++ "_u") (Value (PrimitiveType left) e1) (Value (PrimitiveType right) e2)
signedArithmeticExpr _ _ _ = error "Cannot perform arithmetics on mixed signed/unsigned type"

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
