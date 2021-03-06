module MetroLang.Compilation.Expressions (exprs, expr, strToTypeMaybe) where

import MetroLang.AST as Metro (PrimitiveType (..), Type (..))
import qualified MetroLang.AST as Metro
import MetroLang.Compilation.Combinators
import MetroLang.Compilation.Context
import MetroLang.Compilation.Values
import MetroLang.Types
import qualified MetroLang.WebAssembly.AST as WASM
import MetroLang.WebAssembly.MemoryInstr
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
        refType <- strToTypeMaybe varName
        case refType of
          Just theType -> return $ Value (TypeRef theType) $ i32Const 0
          Nothing -> do
            localVarExpr varName
expr (Metro.BooleanLiteral True) = return trueValue
expr (Metro.BooleanLiteral False) = return falseValue
expr (Metro.NumberLiteral p n)
  | dataTypeToValtype (Metro.Primitive p) == WASM.I64 = return $ Value (Primitive p) $ i64Const n
  | otherwise = return $ Value (Primitive p) $ i32Const n
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
expr (Metro.As e1 e2) = asExpr e1 e2
expr (Metro.Call callee args) =
  do
    a <- arguments args
    isConstructorCall <- classExists callee
    if isConstructorCall
      then return $ Value (Generic callee []) $ call callee (map wasmExpr a)
      else functionCall callee a
expr (Metro.ListAccess obj key) = listAccessExpr obj key
expr (Metro.Match t body) = matchExpr t body
expr Metro.Wildcard = error "Found a lost wildcard."

functionCall :: String -> [Value] -> Compiler Value
functionCall fnName args =
  do
    functionInfo <- lookupFunction fnName
    wasmArgs <- return $ checkFunctionSignature 1 fnName (parameters functionInfo) args
    return $ Value (returnDataType functionInfo) $ call fnName wasmArgs

asExpr :: Metro.Expression -> Metro.Expression -> Compiler Value
asExpr e (Metro.VariableExpr typeIdentifier) =
  do
    actualType <- strToType typeIdentifier
    Value _ f <- expr e
    return $ Value actualType f
asExpr _ _ = error "Cannot cast to the given right-hand side value."

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
    isPrimitive <- return $ strToPrimitiveType str
    isClass <- strToClass str
    return $ isPrimitive ?? isClass

(??) :: Maybe a -> Maybe a -> Maybe a
(Just x) ?? _ = Just x
Nothing ?? f = f

strToPrimitiveType :: String -> Maybe Type
strToPrimitiveType "Bool" = Just $ Primitive TBool
strToPrimitiveType "IntXS" = Just $ Primitive TIntXS
strToPrimitiveType "Byte" = Just $ Primitive TByte
strToPrimitiveType "IntS" = Just $ Primitive TIntS
strToPrimitiveType "Word" = Just $ Primitive TWord
strToPrimitiveType "Int" = Just $ Primitive TInt
strToPrimitiveType "UInt" = Just $ Primitive TUInt
strToPrimitiveType "IntL" = Just $ Primitive TIntL
strToPrimitiveType "UIntL" = Just $ Primitive TUIntL
strToPrimitiveType "Float" = Just $ Primitive TFloat
strToPrimitiveType "FloatL" = Just $ Primitive TFloatL
strToPrimitiveType "Char" = Just $ Primitive TChar
strToPrimitiveType "String" = Just $ Primitive TString
strToPrimitiveType _ = Nothing

strToClass :: String -> Compiler (Maybe Type)
strToClass className =
  do
    isClass <- classExists className
    if isClass
      then return $ Just $ Generic className []
      else return Nothing

localVarExpr :: String -> Compiler Value
localVarExpr varName =
  do
    varType <- lookupVariableType varName
    return $ Value varType $ getLocal varName

unaryExpr :: Metro.UnaryOp -> Metro.Expression -> Compiler Value
unaryExpr op e = expr e >>= \value -> return $ unaryExprWasm op value

unaryExprWasm :: Metro.UnaryOp -> Value -> Value
unaryExprWasm Metro.Neg (Value (Primitive TInt) e) = Value (Primitive TInt) $ i32Sub (i32Const 0) e
unaryExprWasm Metro.Neg _ = error "Cannot negate the given value."
unaryExprWasm Metro.LogicalNot (Value (Primitive TBool) e) = Value (Primitive TBool) $ i32Eqz e
unaryExprWasm Metro.LogicalNot _ = error "Can only apply 'not' on a Bool."
unaryExprWasm Metro.BitwiseNot (Value (Primitive p) e)
  | p == TByte || p == TIntXS = Value (Primitive p) $ i32Xor (i32Const 0xFF) e
  | p == TWord || p == TIntS = Value (Primitive p) $ i32Xor (i32Const 0xFFFF) e
  | p == TUInt || p == TInt = Value (Primitive p) $ i32Xor (i32Const 0xFFFFFFFF) e
  | p == TUIntL || p == TIntL = Value (Primitive p) $ i64Xor (i64Const 0xFFFFFFFFFFFFFFFF) e
unaryExprWasm Metro.BitwiseNot (Value t _) = error $ "Cannot apply '~' on " ++ (show t)

binaryExpr :: Metro.BinOp -> Metro.Expression -> Metro.Expression -> Compiler Value
binaryExpr Metro.Definition e1 e2 = definitionExpr e1 e2
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
fieldAccess (Value (TypeRef (Primitive TInt)) _) "MIN_VALUE" = return $ Value (Primitive TInt) $ i32Const $ 0 - 2147483648
fieldAccess (Value (TypeRef (Primitive TInt)) _) "MAX_VALUE" = return $ Value (Primitive TInt) $ i32Const 2147483647
fieldAccess (Value (Primitive TUInt) obj) "lowWord" = return $ convertTo TUInt TWord obj
fieldAccess (Value (Primitive TUInt) obj) "highWord" = return $ Value (Primitive TWord) $ i32Shru obj $ i32Const 16
fieldAccess (Value (Primitive TString) obj) "length" = return $ Value (Primitive TInt) $ loadInstr TInt 0 obj
fieldAccess (Value (List _) obj) "length" = return $ Value (Primitive TUInt) $ loadInstr TUInt 0 obj
fieldAccess (Value (Generic className _) objExpr) fieldName =
  do
    fieldOffset <- getFieldOffset className fieldName
    return $ Value (Primitive TInt) $ loadInstr TInt fieldOffset objExpr
-- TODO: can only load Int
fieldAccess (Value objType _) methodName = error $ "Unknown field " ++ methodName ++ " on primitive type " ++ show objType

methodCall :: Value -> String -> [Value] -> Compiler Value
methodCall (Value (Primitive p) obj) "toIntXS" [] = return $ convertTo p TIntXS obj
methodCall (Value (Primitive p) obj) "toIntS" [] = return $ convertTo p TIntS obj
methodCall (Value (Primitive p) obj) "toInt" [] = return $ convertTo p TInt obj
methodCall (Value (Primitive p) obj) "toIntL" [] = return $ convertTo p TIntL obj
methodCall (Value (Primitive p) obj) "toByte" [] = return $ convertTo p TByte obj
methodCall (Value (Primitive p) obj) "toWord" [] = return $ convertTo p TWord obj
methodCall (Value (Primitive p) obj) "toUInt" [] = return $ convertTo p TUInt obj
methodCall (Value (Primitive p) obj) "toUIntL" [] = return $ convertTo p TUIntL obj
methodCall (Value (Primitive p) obj) "store" [arg0] = return $ Value TVoid $ storeInstr p 0 (wasmExpr arg0) obj
methodCall (Value (Primitive p) obj) "countLeadingZeros" [] = return $ clz p obj
methodCall (Value (Primitive p) obj) "countTrailingZeros" [] = return $ ctz p obj
methodCall (Value (Primitive p) obj) "countOnes" [] = return $ popcnt p obj
methodCall (Value (Primitive TString) obj) "toByteList" [] = return $ Value (List (Primitive TByte)) $ obj
methodCall (Value (TypeRef typeRef) _) methodName args = staticClassMethodCall typeRef methodName args
methodCall (Value objType obj) methodName args = classMethodCall (show objType) obj methodName args

--methodCall (Value objType _) methodName args = error $ "Unknown method " ++ methodName ++ argsToInfo args ++ " on primitive type " ++ show objType

staticClassMethodCall :: Type -> String -> [Value] -> Compiler Value
staticClassMethodCall (Primitive p) "load" [arg0] = return $ Value (Primitive p) $ loadInstr p 0 (wasmExpr arg0)
staticClassMethodCall (Primitive p) "store" [arg0, arg1] = return $ Value TVoid $ storeInstr p 0 (wasmExpr arg1) (wasmExpr arg0)
staticClassMethodCall typeRef methodName args =
  do
    method <- lookupClassMethod (show typeRef) methodName
    wasmArgs <- return $ checkFunctionSignature 1 methodName (parameters method) args
    return $ Value (returnDataType method) $ call ((show typeRef) ++ "." ++ methodName) wasmArgs

classMethodCall :: String -> WASM.Expr -> String -> [Value] -> Compiler Value
classMethodCall className obj methodName args =
  do
    method <- lookupClassMethod className methodName
    wasmArgs <- return $ checkFunctionSignature 1 methodName (parameters method) args
    return $ Value (returnDataType method) $ call (className ++ "." ++ methodName) $ obj : wasmArgs

clz :: Metro.PrimitiveType -> WASM.Expr -> Value
clz p e
  | p == TByte || p == TIntXS = Value (Primitive TByte) $ WASM.Method "clz" WASM.I32 [i32Shl (i32Const 24) e]
  | p == TWord || p == TIntS = Value (Primitive TByte) $ WASM.Method "clz" WASM.I32 [i32Shl (i32Const 16) e]
  | p == TUInt || p == TInt = Value (Primitive TByte) $ WASM.Method "clz" WASM.I32 [e]
  | p == TUIntL || p == TIntL = Value (Primitive TByte) $ WASM.Method "clz" WASM.I64 [e]
  | otherwise = error $ "Cannot count leading zeros for " ++ (show p)

ctz :: Metro.PrimitiveType -> WASM.Expr -> Value
ctz p e
  | p <= TUInt || p <= TInt = Value (Primitive TByte) $ WASM.Method "ctz" WASM.I32 [e]
  | p == TUIntL || p == TIntL = Value (Primitive TByte) $ WASM.Method "ctz" WASM.I64 [e]
  | otherwise = error $ "Cannot count trailing zeros for " ++ (show p)

popcnt :: Metro.PrimitiveType -> WASM.Expr -> Value
popcnt p e
  | p <= TUInt || p <= TInt = Value (Primitive TByte) $ WASM.Method "popcnt" WASM.I32 [e]
  | p == TUIntL || p == TIntL = Value (Primitive TByte) $ WASM.Method "popcnt" WASM.I64 [e]
  | otherwise = error $ "Cannot count ones for " ++ (show p)

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

matchExpr :: Metro.Expression -> Metro.MatchBody -> Compiler Value
matchExpr target body = matchBody body target

matchBody :: Metro.MatchBody -> Metro.Expression -> Compiler Value
matchBody (Metro.MatchBody cases) = matchCases cases

matchCases :: [Metro.MatchCase] -> Metro.Expression -> Compiler Value
matchCases [] _ = error "There must be at least one case in a match block."
matchCases [Metro.MatchCase Metro.Wildcard caseVal] _ =
  do
    Value valType valExpr <- expr caseVal
    return $ Value valType valExpr
matchCases [_] _ = error "Invalid last condition, it must be a wildcard."
matchCases (c : cs) target =
  do
    Metro.MatchCase caseCond caseVal <- return c
    Value valType valExpr <- expr caseVal
    Value elseType elseExpr <- matchCases cs target
    if elseType /= valType
      then error "Return types of all match cases must be consistent."
      else do
        cond <- makeMatchCaseCond target caseCond
        return $ Value valType $ WASM.Select valExpr elseExpr cond

makeMatchCaseCond :: Metro.Expression -> Metro.Expression -> Compiler WASM.Expr
makeMatchCaseCond left right =
  do
    Value leftType leftExpr <- expr left
    Value rightType rightExpr <- expr right
    if leftType /= rightType
      then error $ "Match case condition type " ++ (show rightType) ++ " does not match type of value to match " ++ (show leftType)
      else return $ i32Eq leftExpr rightExpr

load :: Type -> WASM.Expr -> Value
load (Primitive p) n1 = Value (Primitive p) $ loadInstr p 0 n1
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
assignment left right =
  do
    parsedLeft <- leftHandSideSetter left
    parsedRight <- expr right
    return $ Value TVoid $ parsedLeft (wasmExpr parsedRight)

assignmentOp :: Metro.BinOp -> Metro.Expression -> Metro.Expression -> Compiler Value
assignmentOp op left right =
  do
    setterLeft <- leftHandSideSetter left
    getterLeft <- leftHandSideGetter left
    parsedRight <- expr right
    return $ Value TVoid $ setterLeft $ wasmExpr $ binaryExprWasm op getterLeft parsedRight

leftHandSideGetter :: Metro.Expression -> Compiler Value
leftHandSideGetter (Metro.VariableExpr varName) = localVarExpr varName
leftHandSideGetter (Metro.Binary Metro.Chain Metro.ThisKeyword (Metro.VariableExpr fieldName)) =
  do
    classType <- requireThisContext
    fieldOffset <- getFieldOffset (show classType) fieldName
    return $ Value (Primitive TInt) $ loadInstr TInt fieldOffset (getLocal "this")
leftHandSideGetter _ = error "Not a valid left-hand side assignment expression."

leftHandSideSetter :: Metro.Expression -> Compiler (WASM.Expr -> WASM.Expr)
leftHandSideSetter (Metro.VariableExpr i) = return $ setLocal i
leftHandSideSetter (Metro.Binary Metro.Chain Metro.ThisKeyword (Metro.VariableExpr fieldName)) =
  do
    classType <- requireThisContext
    fieldOffset <- getFieldOffset (show classType) fieldName
    return $ storeInstr TInt fieldOffset (getLocal "this")
leftHandSideSetter _ = error "Not a valid left-hand side assignment expression."

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
binaryExprWasm op _ _ = error $ (show op) ++ " not implemented yet"

--binaryExpr Metro.OptChain e1 e2 = TODO!
--binaryExpr Metro.Chain e1 e2 = TODO!

boolExpr :: String -> Value -> Value -> Value
boolExpr op (Value (Primitive TBool) e1) (Value (Primitive TBool) e2) = Value (Primitive TBool) $ WASM.Method op WASM.I32 [e1, e2]
boolExpr op _ _ = error $ "Can only apply '" ++ op ++ "' on two Bools."

signedComparingExpr :: String -> Value -> Value -> Value
signedComparingExpr op (Value (Primitive left) e1) (Value (Primitive right) e2)
  | isSignedType left && isSignedType right = comparingExpr (op ++ "_s") (Value (Primitive left) e1) (Value (Primitive right) e2)
  | isUnsignedType left && isUnsignedType right = comparingExpr (op ++ "_u") (Value (Primitive left) e1) (Value (Primitive right) e2)
signedComparingExpr _ _ _ = error "Cannot perform signed operation on mixed signed/unsigned type"

comparingExpr :: String -> Value -> Value -> Value
comparingExpr op (Value (Primitive left) e1) (Value (Primitive right) e2)
  | left == right = Value (Primitive TBool) $ WASM.Method op (dataTypeToValtype (Metro.Primitive right)) [e1, e2]
  | left < right = Value (Primitive TBool) $ WASM.Method op (dataTypeToValtype (Metro.Primitive right)) [convertToExpr left right e1, e2]
  | left > right = Value (Primitive TBool) $ WASM.Method op (dataTypeToValtype (Metro.Primitive left)) [e1, convertToExpr right left e2]
comparingExpr op (Value left _) (Value right _) = error $ "Cannot apply " ++ op ++ " on " ++ (show left) ++ " and " ++ (show right) ++ "."

arithmeticExpr :: String -> Value -> Value -> Value
arithmeticExpr op (Value (Primitive left) e1) (Value (Primitive right) e2)
  | left == right = maskTo right $ WASM.Method op (dataTypeToValtype (Metro.Primitive right)) [e1, e2]
  | left < right = maskTo right $ WASM.Method op (dataTypeToValtype (Metro.Primitive right)) [convertToExpr left right e1, e2]
  | left > right = maskTo left $ WASM.Method op (dataTypeToValtype (Metro.Primitive left)) [e1, convertToExpr right left e2]
arithmeticExpr op (Value left _) (Value right _) = error $ "Cannot apply " ++ op ++ " on " ++ (show left) ++ " and " ++ (show right) ++ "."

signedArithmeticExpr :: String -> Value -> Value -> Value
signedArithmeticExpr op (Value (Primitive left) e1) (Value (Primitive right) e2)
  | isSignedType left && isSignedType right = arithmeticExpr (op ++ "_s") (Value (Primitive left) e1) (Value (Primitive right) e2)
  | isUnsignedType left && isUnsignedType right = arithmeticExpr (op ++ "_u") (Value (Primitive left) e1) (Value (Primitive right) e2)
signedArithmeticExpr _ _ _ = error "Cannot perform signed operation on mixed signed/unsigned type"

maskTo :: Metro.PrimitiveType -> WASM.Expr -> Value
maskTo dest = (Value $ Metro.Primitive dest) . (maskToExpr dest)

maskToExpr :: Metro.PrimitiveType -> WASM.Expr -> WASM.Expr
maskToExpr TByte = i32And (i32Const 0xFF)
maskToExpr TIntXS = i32And (i32Const 0xFF)
maskToExpr TWord = i32And (i32Const 0xFFFF)
maskToExpr TIntS = i32And (i32Const 0xFFFF)
maskToExpr _ = id

convertTo :: Metro.PrimitiveType -> Metro.PrimitiveType -> WASM.Expr -> Value
convertTo src dest = (Value $ Metro.Primitive dest) . (convertToExpr src dest)

convertToExpr :: Metro.PrimitiveType -> Metro.PrimitiveType -> WASM.Expr -> WASM.Expr
convertToExpr src TUIntL
  | src <= TInt = i64ExtendI32U
  | src <= TUInt = i64ExtendI32U
  | otherwise = error $ "Cannot convert " ++ (show src) ++ " to UIntL"
convertToExpr src TIntL
  | src <= TInt = i64ExtendI32S
  | src <= TUInt = i64ExtendI32S
  | otherwise = error $ "Cannot convert " ++ (show src) ++ " to IntL"
convertToExpr src dest
  | src == dest = id
  | src < dest = id
  | src > dest = maskToExpr dest
  | src == unsigned dest = id
  | otherwise = error $ "Cannot convert " ++ (show src) ++ " to " ++ (show dest)

arguments :: Metro.Arguments -> Compiler [Value]
arguments (Metro.Args e) = exprs e
