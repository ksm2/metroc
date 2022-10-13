module MetroLang.WebAssembly.AST where

import MetroLang.Bytes

newtype Module = Mod [Declaration] deriving (Show)

data Declaration
  = Import StringLiteral StringLiteral ImportSpecifier
  | Memory Identifier Integer
  | Export StringLiteral ExportSpecifier
  | Global Identifier Globaltype Expr
  | Data Expr Bytes
  | Func Identifier FuncExport [Param] ReturnType [Expr]
  | Start Identifier
  deriving (Show)

instance Eq Declaration where
  (Import a1 b1 _) == (Import a2 b2 _) = a1 == a2 && b1 == b2
  (Memory a1 _) == (Memory a2 _) = a1 == a2
  (Export a1 _) == (Export a2 _) = a1 == a2
  (Global a1 _ _) == (Global a2 _ _) = a1 == a2
  (Func a1 _ _ _ _) == (Func a2 _ _ _ _) = a1 == a2
  (Start a1) == (Start a2) = a1 == a2
  _ == _ = False

instance Ord Declaration where
  Import {} <= Import {} = True
  Import {} <= (Memory _ _) = True
  Import {} <= (Export _ _) = True
  Import {} <= Global {} = True
  Import {} <= (Data _ _) = True
  Import {} <= Func {} = True
  Import {} <= (Start _) = True
  (Memory _ _) <= (Memory _ _) = True
  (Memory _ _) <= (Export _ _) = True
  (Memory _ _) <= Global {} = True
  (Memory _ _) <= (Data _ _) = True
  (Memory _ _) <= Func {} = True
  (Memory _ _) <= (Start _) = True
  (Export _ _) <= (Export _ _) = True
  (Export _ _) <= Global {} = True
  (Export _ _) <= (Data _ _) = True
  (Export _ _) <= Func {} = True
  (Export _ _) <= (Start _) = True
  Global {} <= Global {} = True
  Global {} <= (Data _ _) = True
  Global {} <= Func {} = True
  Global {} <= (Start _) = True
  (Data _ _) <= (Data _ _) = True
  (Data _ _) <= Func {} = True
  (Data _ _) <= (Start _) = True
  Func {} <= Func {} = True
  Func {} <= (Start _) = True
  (Start _) <= (Start _) = True
  _ <= _ = False

data ImportSpecifier = IFunc Identifier [Param] ReturnType
  deriving (Show)

data ExportSpecifier
  = EMemory Identifier
  | EFunc Identifier
  deriving (Show)

type FuncExport = Maybe String

data Expr
  = Local Identifier Valtype
  | Block Identifier ReturnType [Expr]
  | Loop Identifier [Expr]
  | Return Expr
  | Instr String [Expr]
  | Method String Valtype [Expr]
  | MemoryInstr String Valtype Offset Align [Expr]
  | Select Expr Expr Expr
  | Lit Integer
  | Var Identifier
  deriving (Show)

type Offset = Maybe Integer

type Align = Maybe Integer

data Param
  = Par Identifier Valtype
  | AnonymousPar Valtype
  deriving (Show)

type ReturnType = Maybe Result

newtype Result = Res Valtype deriving (Show)

type Identifier = String

type StringLiteral = String

data Globaltype = Imut Valtype | Mut Valtype deriving (Show)

data Valtype = I32 | I64 | F32 | F64 deriving (Show, Eq)
