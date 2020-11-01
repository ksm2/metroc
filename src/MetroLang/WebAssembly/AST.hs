module MetroLang.WebAssembly.AST where

import MetroLang.Bytes

data Module = Mod [Declaration] deriving (Show)

data Declaration = Import StringLiteral StringLiteral ImportSpecifier
                 | Memory Identifier Integer
                 | Export StringLiteral ExportSpecifier
                 | Global Identifier Globaltype Expr
                 | Data Expr Bytes
                 | Func Identifier [Param] ReturnType Stmt
                 | Start Identifier
                   deriving (Show)

instance Eq Declaration where
  (Import a1 b1 _) == (Import a2 b2 _) = a1 == a2 && b1 == b2
  (Memory a1 _) == (Memory a2 _) = a1 == a2
  (Export a1 _) == (Export a2 _) = a1 == a2
  (Global a1 _ _) == (Global a2 _ _) = a1 == a2
  (Func a1 _ _ _) == (Func a2 _ _ _) = a1 == a2
  (Start a1) == (Start a2) = a1 == a2
  _ == _ = False

instance Ord Declaration where
  (Import _ _ _) <= (Import _ _ _) = True
  (Import _ _ _) <= (Memory _ _) = True
  (Import _ _ _) <= (Export _ _) = True
  (Import _ _ _) <= (Global _ _ _) = True
  (Import _ _ _) <= (Data _ _) = True
  (Import _ _ _) <= (Func _ _ _ _) = True
  (Import _ _ _) <= (Start _) = True

  (Memory _ _) <= (Memory _ _) = True
  (Memory _ _) <= (Export _ _) = True
  (Memory _ _) <= (Global _ _ _) = True
  (Memory _ _) <= (Data _ _) = True
  (Memory _ _) <= (Func _ _ _ _) = True
  (Memory _ _) <= (Start _) = True

  (Export _ _) <= (Export _ _) = True
  (Export _ _) <= (Global _ _ _) = True
  (Export _ _) <= (Data _ _) = True
  (Export _ _) <= (Func _ _ _ _) = True
  (Export _ _) <= (Start _) = True

  (Global _ _ _) <= (Global _ _ _) = True
  (Global _ _ _) <= (Data _ _) = True
  (Global _ _ _) <= (Func _ _ _ _) = True
  (Global _ _ _) <= (Start _) = True

  (Data _ _) <= (Data _ _) = True
  (Data _ _) <= (Func _ _ _ _) = True
  (Data _ _) <= (Start _) = True

  (Func _ _ _ _) <= (Func _ _ _ _) = True
  (Func _ _ _ _) <= (Start _) = True

  (Start _) <= (Start _) = True
  _ <= _ = False

data ImportSpecifier = IFunc Identifier [Param] ReturnType
                       deriving (Show)

data ExportSpecifier = EMemory Identifier
                       deriving (Show)

data Stmt = Local Identifier Valtype
          | Block Identifier [Stmt]
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

type ReturnType = Maybe Result

data Result = Res Valtype deriving (Show)

type Identifier = String

type StringLiteral = String

data Globaltype = Imut Valtype | Mut Valtype deriving (Show)

data Valtype = I32 | I64 | F32 | F64 deriving (Show)
