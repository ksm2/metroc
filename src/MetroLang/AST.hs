module MetroLang.AST where

data Module = Mod [Declaration] deriving (Show)

data Declaration = Import String ImportSpecifier
                 | Const Identifier Expression
                 | Enumeration Identifier TypeArgs [EnumItem]
                 | Interface Identifier TypeArgs InterfaceExtends InterfaceBlock
                 | Class Identifier TypeArgs Params ClassExtends Implements ClassBlock
                 | Impl Type Type ClassBlock
                 | Func Identifier Params ReturnType Block
                   deriving (Show)

type Params = [Param]

data Param = Par Identifier Type deriving (Show)

type ReturnType = Type

data ImportSpecifier = FuncImport Identifier Params ReturnType
                       deriving (Show)

-- Enums
data EnumItem = EnumItem Identifier Params deriving Show

-- Interfaces
type InterfaceExtends = [Type]
data InterfaceBlock = InterfaceBlock [MethodSignature] deriving Show

-- Classes
type ClassExtends = Type
type Implements = [Type]
data ClassBlock = ClassBlock [Method] deriving Show
data MethodSignature = MethodSignature Identifier Params ReturnType deriving (Show)
data Method = Method MethodSignature Block deriving Show

data Block = Block [Stmt] deriving (Show)

data Stmt = IfStmt If
          | WhileStmt Expression Block
          | ReturnStmt Expression
          | ExprStmt Expression
            deriving (Show)

data If = If Expression Block (Maybe Else) deriving (Show)

data Else = ElseStmt Block
          | ElseIfStmt If
            deriving (Show)

data BinOp = Assignment
           | Definition
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
           | UnsignedShiftRight
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
                | NumberLiteral PrimitiveType Integer
                | StringLiteral String
                | NullLiteral
                | ThisKeyword
                | Unary UnaryOp Expression
                | Binary BinOp Expression Expression
                | Call Identifier Arguments
                  deriving (Show)

data Arguments = Args [Expression] deriving (Show)

type Identifier = String

-- Types
type TypeArgs = [Type]
data Type = TVoid
          | Primitive PrimitiveType
          | Generic String TypeArgs
data PrimitiveType  = TBool
                    | TByte
                    | TUByte
                    | TWord
                    | TUWord
                    | TInt
                    | TUInt
                    | TLong
                    | TULong
                    | TFloat
                    | TDouble
                    | TChar
                    | TString
                      deriving (Enum, Show, Eq)

instance Eq Type where
  TVoid == TVoid = True
  Primitive a == Primitive b = a == b
  Generic a1 a2 == Generic b1 b2 = a1 == b1 && a2 == b2
  _ == _ = False

instance Show Type where
  show TVoid = "Void"
  show (Primitive p) = let (_:xs) = show p in xs
  show (Generic s []) = s
  show (Generic s args) = s ++ "<" ++ (joinArgs args) ++ ">"

joinArgs :: (Show a) => [a] -> String
joinArgs [] = ""
joinArgs [element] = show element
joinArgs (x:xs) = (show x) ++ ", " ++ (joinArgs xs)
