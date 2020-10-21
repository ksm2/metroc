module MetroLang.AST where

data Module = Mod [Declaration] deriving (Show)

data Declaration = Func Identifier Params Block
                   deriving (Show)

type Params = [Param]

data Param = Par Identifier Type deriving (Show)

data Block = Block [Stmt] deriving (Show)

data Stmt = IfStmt If
          | ExprStmt Expression
            deriving (Show)

data If = If Expression Block (Maybe Else) deriving (Show)

data Else = ElseStmt Block
          | ElseIfStmt If
            deriving (Show)

data BinOp = LogicalOr
           | LogicalAnd
           | Subtract
           | Add
           | Modulo
           | Divide
           | Multiply
             deriving (Show)

data Expression = VariableExpr Identifier
                | BooleanLiteral Bool
                | NumberLiteral Integer
                | StringLiteral String
                | NullLiteral
                | Neg Expression
                | Binary BinOp Expression Expression
                  deriving (Show)

type Identifier = String

type Type = String
