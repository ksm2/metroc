module MetroLang.Lang.Model where

import MetroLang.Location

type Identifier = String

type ModuleName = String

newtype Module
  = Module [Declaration]
  deriving (Show)

data Safety = Safe | Unsafe deriving (Show, Eq)

data Declaration
  = ImportDeclaration FQN
  | ExportDeclaration Declaration
  | TestDeclaration Identifier [TestStatement]
  | HideDeclaration Declaration
  | ConstDeclaration Identifier Expression
  | ExternalDeclaration ModuleName External
  | EnumDeclaration Identifier TypeArguments EnumItems
  | InterfaceDeclaration Identifier TypeArguments InterfaceMethods
  | ImplDeclaration Type Type ClassBody
  | ClassDeclaration Identifier TypeArguments Params Types Types ClassBody
  | FnDeclaration Identifier Safety Params ReturnType Block
  deriving (Show)

data TestStatement
  = TestStatement String Block
  deriving (Show)

data External = FnExternal Identifier Params ReturnType
  deriving (Show)

type EnumItems = [EnumItem]

data EnumItem
  = EnumItem Identifier Params
  deriving (Show)

type InterfaceMethods = [InterfaceMethod]

data InterfaceMethod
  = InterfaceMethod Identifier Arguments ReturnType
  deriving (Show)

type ClassBody = [ClassElement]

data ClassElement
  = Method MethodSignature Block
  | StaticMethod MethodSignature Block
  | Field String Expression
  | StaticField String Expression
  deriving (Show)

data MethodSignature
  = MethodSignature Safety Identifier Params ReturnType
  deriving (Show)

type Block = [Statement]

data Statement
  = AssignStatement Var Expression
  | IfStatement If
  | WhileStatement Expression Block
  | AssertStatement Expression String
  | ExpressionStatement Expression
  | ReturnStatement Expression (Maybe Expression)
  | UnsafeStatement Block
  deriving (Show)

data If
  = If Expression Block (Maybe Else)
  deriving (Show)

data Else
  = ElseIf If
  | Else Block
  deriving (Show)

type Var = Identifier

type Params = [Param]

data Param
  = Param Identifier Type
  deriving (Show)

type TypeArguments = [TypeArgument]

newtype TypeArgument = TypeArgument Identifier
  deriving (Eq, Show)

type ReturnType = Type

type Types = [Type]

data TypeSymbol = TypeSymbol {typeSymbolType :: Type, typeSymbolLoc :: SourceLocation} deriving (Show)

data Type
  = VoidType
  | RefType Identifier
  | MetaType Type
  | PrimitiveType PrimitiveType
  | ArrayType Type
  | GenericType Type TypeArguments

instance Eq Type where
  VoidType == VoidType = True
  RefType a == RefType b = a == b
  MetaType a == MetaType b = a == b
  PrimitiveType a == PrimitiveType b = a == b
  ArrayType a == ArrayType b = a == b
  GenericType a1 a2 == GenericType b1 b2 = a1 == b1 && a2 == b2
  _ == _ = False

instance Show Type where
  show VoidType = "Void"
  show (RefType s) = s
  show (MetaType t) = "type of type " ++ show t
  show (PrimitiveType p) = let (_ : xs) = show p in xs
  show (ArrayType t) = "[" ++ show t ++ "]"
  show (GenericType s []) = show s
  show (GenericType s args) = show s ++ "<" ++ joinArgs args ++ ">"

joinArgs :: (Show a) => [a] -> String
joinArgs [] = ""
joinArgs [element] = show element
joinArgs (x : xs) = show x ++ ", " ++ joinArgs xs

data PrimitiveType
  = TBool
  | TIntXS
  | TByte
  | TIntS
  | TWord
  | TInt
  | TUInt
  | TIntL
  | TUIntL
  | TFloat
  | TFloatL
  | TChar
  | TString
  deriving (Bounded, Enum, Show, Eq)

instance Ord PrimitiveType where
  a <= b
    | a == b = True
    | a == TIntXS = b >= TIntS
    | a == TIntS = b >= TInt
    | a == TInt = b >= TIntL
    | a == TByte = b >= TWord
    | a == TWord = b >= TUInt
    | a == TUInt = b >= TUIntL
    | a == TFloat = b >= TFloatL
    | otherwise = False

type FQN = [Identifier]

data Arguments = Arguments {argumentsExpressions :: [Expression], argumentsLoc :: SourceLocation} deriving (Show)

type Expressions = [Expression]

data Expression
  = ParenExpression Expression
  | LiteralExpression {exprLiteral :: Literal, loc :: SourceLocation}
  | VarExpression {var :: Var, loc :: SourceLocation}
  | ThisExpression {loc :: SourceLocation}
  | NullExpression {loc :: SourceLocation}
  | CastExpression {expr :: Expression, exprType :: Type, loc :: SourceLocation}
  | CallExpression Identifier Arguments
  | MethodCallExpression Expression Identifier Arguments
  | AccessExpression Expression Identifier
  | TypeExpression {exprType :: Type, loc :: SourceLocation}
  | IndexExpression Expression Expression
  | MatchExpression Expression MatchRules
  | UnaryExpression UnaryOperator Expression
  | BinaryExpression BinaryOperator Expression Expression
  deriving (Show)

data Literal
  = IntLiteral {litInt :: Int, litLoc :: SourceLocation}
  | UIntLiteral {litInt :: Int, litLoc :: SourceLocation}
  | ByteLiteral {litInt :: Int, litLoc :: SourceLocation}
  | StringLiteral {litStr :: String, litLoc :: SourceLocation}
  | BoolLiteral {litBool :: Bool, litLoc :: SourceLocation}
  deriving (Show)

type MatchRules = [MatchRule]

data MatchRule
  = MatchRule MatchCondition Expression
  deriving (Show)

data MatchCondition
  = MatchWildcard
  | MatchPattern Literal
  deriving (Show)

data UnaryOperator
  = Neg
  | LogicalNot
  | BitwiseNot
  deriving (Show)

data BinaryOperator
  = Assignment
  | AssignBitwiseOr
  | AssignBitwiseXor
  | AssignBitwiseAnd
  | AssignRotateLeft
  | AssignRotateRight
  | AssignShiftLeft
  | AssignShiftRight
  | AssignSubtract
  | AssignAdd
  | AssignModulo
  | AssignDivide
  | AssignMultiply
  | LogicalOr
  | LogicalAnd
  | BitwiseOr
  | BitwiseXor
  | BitwiseAnd
  | Unequal
  | Equal
  | Is
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
  | RotateLeft
  | RotateRight
  | ShiftLeft
  | ShiftRight
  | Subtract
  | Add
  | Modulo
  | Divide
  | Multiply
  deriving (Show)
