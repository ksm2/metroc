module MetroLang.AST where

data Module = Mod [Declaration] deriving (Show)

data Declaration = Import String ImportSpecifier
                 | Enumeration Identifier [EnumItem]
                 | Class Identifier Params ClassBlock
                 | Func Identifier Params ReturnType Block
                   deriving (Show)

type Params = [Param]

data Param = Par Identifier Type deriving (Show)

type ReturnType = Maybe Type

data ImportSpecifier = FuncImport Identifier Params ReturnType
                       deriving (Show)

data EnumItem = EnumItem Identifier Params
                deriving (Show)

data ClassBlock = ClassBlock [Method] deriving (Show)

data Method = Method Identifier Params ReturnType Block deriving (Show)

data Block = Block [Stmt] deriving (Show)

data Stmt = IfStmt If
          | ReturnStmt Expression
          | ExprStmt Expression
            deriving (Show)

data If = If Expression Block (Maybe Else) deriving (Show)

data Else = ElseStmt Block
          | ElseIfStmt If
            deriving (Show)

data BinOp = Assignment
           | Definition
           | Is
           | Unequal
           | Equal
           | LessThan
           | LessThanOrEqual
           | GreaterThan
           | GreaterThanOrEqual
           | LogicalOr
           | LogicalAnd
           | Subtract
           | Add
           | Modulo
           | Divide
           | Multiply
           | OptChain
           | Chain
             deriving (Show)

data UnaryOp = Neg
             | LogicalNot
              deriving (Show)

data Expression = VariableExpr Identifier
                | BooleanLiteral Bool
                | NumberLiteral Integer
                | StringLiteral String
                | NullLiteral
                | ThisKeyword
                | Unary UnaryOp Expression
                | Binary BinOp Expression Expression
                | Call Identifier Arguments
                  deriving (Show)

data Arguments = Args [Expression] deriving (Show)

type Identifier = String

type Type = String
