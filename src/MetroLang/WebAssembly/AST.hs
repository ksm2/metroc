module MetroLang.WebAssembly.AST where

data Module = Mod [Declaration] deriving (Show)

data Declaration = Import StringLiteral StringLiteral ImportSpecifier
                 | Memory Identifier Integer
                 | Export StringLiteral ExportSpecifier
                 | Data Expr StringLiteral
                 | Func Identifier [Param] Stmt
                 | Start Identifier
                   deriving (Show)

data ImportSpecifier = IFunc Identifier [Param] Result
                       deriving (Show)

data ExportSpecifier = EMemory Identifier
                       deriving (Show)

data Stmt = Local Identifier Valtype
          | Block Identifier Stmt
          | Exp Expr
          | Seq [Stmt]
            deriving (Show)

data Expr = Instr String [Expr]
          | Method String Valtype [Expr]
          | Lit Integer
          | Var Identifier
            deriving (Show)

data Param = Par Identifier Valtype
           | AnonymousPar Valtype
             deriving (Show)

data Result = Res Valtype deriving (Show)

type Identifier = String

type StringLiteral = String

data Valtype = I32 | I64 | F32 | F64 deriving (Show)
