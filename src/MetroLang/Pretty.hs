module MetroLang.Pretty where

import MetroLang.AST

class Pretty p where
  pretty :: p -> String

instance Pretty Expression where
  pretty (VariableExpr i) = i
  pretty (BooleanLiteral True) = "true"
  pretty (BooleanLiteral False) = "false"
  pretty (NumberLiteral TInt num) = show num
  pretty (NumberLiteral TUInt num) = show num ++ "U"
  pretty (NumberLiteral TByte num) = show num ++ "B"
  pretty (StringLiteral str) = "\"" ++ str ++ "\""
  pretty NullLiteral = "null"
  pretty ThisKeyword = "this"
  pretty (Unary unaryOp expr) = (pretty unaryOp) ++ (pretty expr)
  pretty (Binary Chain left right) = (pretty left) ++ "." ++ (pretty right)
  pretty (Binary OptChain left right) = (pretty left) ++ "?." ++ (pretty right)
  pretty (Binary binOp left right) = (pretty left) ++ " " ++ (pretty binOp) ++ " " ++ (pretty right)
  pretty (Call fn args) = fn ++ (pretty args)
  pretty (ListAccess obj idx) = (pretty obj) ++ "[" ++ (pretty idx) ++ "]"
  pretty (As left right) = (pretty left) ++ " as " ++ (pretty right)
  pretty Wildcard = "_"
  pretty _ = ""

instance Pretty Arguments where
  pretty (Args ee) = "(" ++ prettyArgs ee ++ ")"

prettyArgs :: (Pretty a) => [a] -> String
prettyArgs [] = ""
prettyArgs [element] = pretty element
prettyArgs (x : xs) = (pretty x) ++ ", " ++ (prettyArgs xs)

instance Pretty UnaryOp where
  pretty Neg = "-"
  pretty LogicalNot = "not "
  pretty BitwiseNot = "~"

instance Pretty BinOp where
  pretty Assignment = "="
  pretty Definition = ":="
  pretty AssignBitwiseOr = "|="
  pretty AssignBitwiseXor = "^="
  pretty AssignBitwiseAnd = "&="
  pretty AssignRotateLeft = "<%="
  pretty AssignRotateRight = "%>="
  pretty AssignShiftLeft = "<<="
  pretty AssignShiftRight = ">>="
  pretty AssignSubtract = "-="
  pretty AssignAdd = "+="
  pretty AssignModulo = "%="
  pretty AssignDivide = "/="
  pretty AssignMultiply = "*="
  pretty LogicalOr = "or"
  pretty LogicalAnd = "and"
  pretty BitwiseOr = "|"
  pretty BitwiseXor = "^"
  pretty BitwiseAnd = "&"
  pretty Unequal = "!="
  pretty Equal = "=="
  pretty Is = "is"
  pretty LessThan = "<"
  pretty LessThanOrEqual = "<="
  pretty GreaterThan = ">"
  pretty GreaterThanOrEqual = ">="
  pretty RotateLeft = "<%"
  pretty RotateRight = "%>"
  pretty ShiftLeft = "<<"
  pretty ShiftRight = ">>"
  pretty Subtract = "-"
  pretty Add = "+"
  pretty Modulo = "%"
  pretty Divide = "/"
  pretty Multiply = "*"
  pretty OptChain = "?."
  pretty Chain = "."
