module MetroLang.Lang.Pretty where

import MetroLang.Lang.Model

class Pretty p where
  pretty :: p -> String

instance Pretty Expression where
  pretty = \case
    ParenExpression e -> "(" ++ pretty e ++ ")"
    LiteralExpression lit -> pretty lit
    VarExpression i -> i
    ThisExpression -> "this"
    NullExpression -> "null"
    CastExpression left right -> pretty left ++ " as " ++ pretty right
    CallExpression fn args -> fn ++ pretty args
    MethodCallExpression obj method args -> pretty obj ++ "." ++ method ++ pretty args
    AccessExpression left right -> pretty left ++ "." ++ right
    TypeExpression typ -> pretty typ
    IndexExpression obj idx -> pretty obj ++ "[" ++ pretty idx ++ "]"
    UnaryExpression unaryOp expr -> pretty unaryOp ++ pretty expr
    BinaryExpression binOp left right -> pretty left ++ " " ++ pretty binOp ++ " " ++ pretty right
    _ -> ""

instance Pretty Literal where
  pretty = \case
    IntLiteral num -> show num
    UIntLiteral num -> show num ++ "U"
    ByteLiteral num -> show num ++ "B"
    StringLiteral str -> "\"" ++ str ++ "\""
    BoolLiteral True -> "true"
    BoolLiteral False -> "false"

instance Pretty Type where
  pretty = \case
    RefType s -> s
    PrimitiveType p -> pretty p
    _ -> ""

instance Pretty PrimitiveType where
  pretty = tail . show

instance Pretty Arguments where
  pretty (Arguments ee) = "(" ++ prettyArgs ee ++ ")"

prettyArgs :: (Pretty a) => [a] -> String
prettyArgs [] = ""
prettyArgs [element] = pretty element
prettyArgs (x : xs) = pretty x ++ ", " ++ prettyArgs xs

instance Pretty UnaryOperator where
  pretty Neg = "-"
  pretty LogicalNot = "not "
  pretty BitwiseNot = "~"

instance Pretty BinaryOperator where
  pretty Assignment = "="
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
