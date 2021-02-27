module MetroLang.Lang.Model where

data Module
  = Module [Declaration]
  deriving (Show)

data Declaration
  = ImportDeclaration FQN
  | ConstDeclaration String Expression
  | EnumDeclaration String TypeArguments EnumItems
  | InterfaceDeclaration String TypeArguments InterfaceMethods
  | FnDeclaration String Arguments ReturnType Statements
  deriving (Show)

type EnumItems = [EnumItem]

data EnumItem
  = EnumItem String Arguments
  deriving (Show)

type InterfaceMethods = [InterfaceMethod]

data InterfaceMethod
  = InterfaceMethod String Arguments ReturnType
  deriving (Show)

type Statements = [Statement]

data Statement
  = AssignStatement Vars Expression
  | ExpressionStatement Expression
  deriving (Show)

type Vars = [Var]

type Var = String

type Arguments = [Argument]

data Argument
  = Argument String Type
  deriving (Show)

type TypeArguments = [TypeArgument]

data TypeArgument
  = TypeArgument String
  deriving (Show)

type ReturnType = [Type]

data Type
  = RefType String
  | ArrayType Type
  deriving (Show)

type FQN = [String]

data Expression
  = LiteralExpression Literal
  | VarExpression Var
  | AccessExpression Expression Var
  | BinaryExpression BinaryOperator Expression Expression
  deriving (Show)

data Literal
  = IntLiteral Int
  | StringLiteral String
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
  | As
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
