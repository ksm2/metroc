module MetroLang.Lang.Model where

data Module
  = Module [Declaration]
  deriving (Show)

data Static = Static | Instance deriving (Show, Eq)

data Safety = Safe | Unsafe deriving (Show, Eq)

data Declaration
  = ImportDeclaration FQN
  | ConstDeclaration String Expression
  | EnumDeclaration String TypeArguments EnumItems
  | InterfaceDeclaration String TypeArguments InterfaceMethods
  | ImplDeclaration Type Type ClassMethods
  | ClassDeclaration String TypeArguments ClassMethods
  | FnDeclaration String Safety Arguments ReturnType Statements
  deriving (Show)

type EnumItems = [EnumItem]

data EnumItem
  = EnumItem String Arguments
  deriving (Show)

type InterfaceMethods = [InterfaceMethod]

data InterfaceMethod
  = InterfaceMethod String Arguments ReturnType
  deriving (Show)

type ClassMethods = [ClassMethod]

data ClassMethod
  = ClassMethod String Static Safety Arguments ReturnType Statements
  deriving (Show)

type Statements = [Statement]

data Statement
  = AssignStatement Vars Expression
  | IfStatement If
  | WhileStatement Expression Statements (Maybe Else)
  | AssertStatement Expression (Maybe String)
  | ExpressionStatement Expression
  | ReturnStatement Expression (Maybe Expression)
  | UnsafeStatement Statements
  deriving (Show)

data If
  = If Expression Statements (Maybe Else)
  deriving Show

data Else
  = ElseIf If
  | Else Statements
  deriving Show

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
  | ArgumentType Type TypeArguments
  deriving (Show)

type FQN = [String]

type Params = [Expression]

data Expression
  = LiteralExpression Literal
  | VarExpression Var
  | ThisExpression
  | CallExpression Expression Params
  | AccessExpression Expression Access
  | UnaryExpression UnaryOperator Expression
  | BinaryExpression BinaryOperator Expression Expression
  deriving (Show)

data Access
  = Access Var (Maybe Access)
  | OptAccess Var (Maybe Access)
  deriving (Show)

data Literal
  = IntLiteral Int
  | StringLiteral String
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
