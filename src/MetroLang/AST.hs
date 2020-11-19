module MetroLang.AST where

data Module = Mod [Declaration] deriving (Show)

data Declaration
  = Import String ImportSpecifier
  | Const Identifier Expression
  | Enumeration Identifier TypeArgs [EnumItem]
  | Interface Identifier TypeArgs InterfaceExtends InterfaceBlock
  | Class Identifier TypeArgs Params ClassExtends Implements ClassBody
  | Impl Type Type ClassBody
  | Func Safety Identifier Params ReturnType Block
  deriving (Show)

data Safety = Safe | Unsafe deriving (Show, Eq)

type Params = [Param]

data Param = Par Identifier Type deriving (Show)

type ReturnType = Type

data ImportSpecifier = FuncImport Identifier Params ReturnType
  deriving (Show)

-- Enums
data EnumItem = EnumItem Identifier Params deriving (Show)

-- Interfaces
type InterfaceExtends = [Type]

data InterfaceBlock = InterfaceBlock [MethodSignature] deriving (Show)

-- Classes
type ClassExtends = Type

type Implements = [Type]

data ClassBody = ClassBody [ClassBodyDeclaration] deriving (Show)

data ClassBodyDeclaration
  = Method MethodSignature Block
  | StaticMethod MethodSignature Block
  | Field Identifier Expression
  deriving (Show)

data MethodSignature = MethodSignature Safety Identifier Params ReturnType deriving (Show)

data Block = Block [Stmt] deriving (Show)

data Stmt
  = IfStmt If
  | WhileStmt Expression Block
  | ReturnStmt Expression (Maybe Expression)
  | ExprStmt Expression
  | UnsafeStmt Block
  deriving (Show)

data If = If Expression Block (Maybe Else) deriving (Show)

data Else
  = ElseStmt Block
  | ElseIfStmt If
  deriving (Show)

data BinOp
  = Assignment
  | Definition
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
  | OptChain
  | Chain
  deriving (Show)

data UnaryOp
  = Neg
  | LogicalNot
  | BitwiseNot
  deriving (Show)

data Expression
  = VariableExpr Identifier
  | BooleanLiteral Bool
  | NumberLiteral PrimitiveType Integer
  | StringLiteral String
  | NullLiteral
  | ThisKeyword
  | Unary UnaryOp Expression
  | Binary BinOp Expression Expression
  | Call Identifier Arguments
  | ListAccess Expression Expression
  | As Expression Expression
  | Match Expression MatchBody
  | Wildcard
  deriving (Show)

data MatchBody = MatchBody [MatchCase] deriving (Show)

data MatchCase = MatchCase Expression Expression deriving (Show)

data Arguments = Args [Expression] deriving (Show)

type Identifier = String

-- Types
type TypeArgs = [Type]

data Type
  = TVoid
  | Primitive PrimitiveType
  | List Type
  | Generic String TypeArgs
  | TypeRef Type

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

instance Eq Type where
  TVoid == TVoid = True
  Primitive a == Primitive b = a == b
  List a == List b = a == b
  Generic a1 a2 == Generic b1 b2 = a1 == b1 && a2 == b2
  TypeRef a == TypeRef b = a == b
  _ == _ = False

instance Show Type where
  show TVoid = "Void"
  show (Primitive p) = let (_ : xs) = show p in xs
  show (List t) = "[" ++ (show t) ++ "]"
  show (Generic s []) = s
  show (Generic s args) = s ++ "<" ++ (joinArgs args) ++ ">"
  show (TypeRef s) = (show s) ++ " ref"

joinArgs :: (Show a) => [a] -> String
joinArgs [] = ""
joinArgs [element] = show element
joinArgs (x : xs) = (show x) ++ ", " ++ (joinArgs xs)
