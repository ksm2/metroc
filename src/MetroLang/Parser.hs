module MetroLang.Parser (parseString, merge) where

import Control.Monad (liftM)
import Data.Functor.Identity
import MetroLang.AST
import MetroLang.Pretty
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDef :: GenLanguageDef String u Identity
languageDef =
  emptyDef
    { Token.commentStart = "/*",
      Token.commentEnd = "*/",
      Token.commentLine = "//",
      Token.identStart = letter <|> oneOf "_",
      Token.identLetter = alphaNum <|> oneOf "_",
      Token.reservedNames =
        [ "and",
          "as",
          "assert",
          "class",
          "const",
          "else",
          "enum",
          "extends",
          "false",
          "fn",
          "for",
          "if",
          "it",
          "impl",
          "import",
          "interface",
          "is",
          "match",
          "not",
          "null",
          "or",
          "return",
          "static",
          "test",
          "this",
          "true",
          "unsafe",
          "while"
        ],
      Token.reservedOpNames =
        [ ",",
          ".",
          "?.",
          "(",
          ")",
          "{",
          "}",
          "~",
          "&",
          "^",
          "|",
          "+",
          "-",
          "*",
          "/",
          "%",
          "=",
          "==",
          "=>",
          "!=",
          ">=",
          ">",
          ">>",
          "<",
          "<<",
          "%>",
          "<%",
          "*=",
          "/=",
          "%=",
          "+=",
          "-=",
          ">>=",
          "<<=",
          "%>=",
          "<%=",
          "&=",
          "^=",
          "|=",
          "_"
        ]
    }

lexer :: Token.GenTokenParser String u Identity
lexer = Token.makeTokenParser languageDef

lexeme :: Parser a -> Parser a
lexeme = Token.lexeme lexer

nbLexeme :: Parser a -> Parser a
nbLexeme p = do x <- p; nbWhiteSpace; return x

identifier :: Parser Identifier
identifier = Token.identifier lexer

-- | reserved parses a reserved word
reserved :: String -> Parser ()
reserved name =
  nbLexeme $
    try $
      do
        _ <- string name
        notFollowedBy (Token.identLetter languageDef) <?> ("end of " ++ show name)

-- | reservedOp parses an operator
reservedOp :: String -> Parser ()
reservedOp name =
  nbLexeme $
    try $
      do
        _ <- string name
        notFollowedBy (Token.opLetter languageDef) <?> ("end of " ++ show name)

-- | symbol parses a symbol with trailing whitespace
symbol :: String -> Parser String
symbol = Token.symbol lexer

-- | parens parses round brackets ("(" and ")") around an expression
parens :: Parser a -> Parser a
parens = Token.parens lexer

-- | braces parses curly brackets ("{" and "}") around an expression
braces :: Parser a -> Parser a
braces = Token.braces lexer

-- | brackets parses square brackets ("[" and "]") around an expression
brackets :: Parser a -> Parser a
brackets = Token.brackets lexer

-- | angles parses angle brackets ("<" and ">") around an expression
angles :: Parser a -> Parser a
angles = Token.angles lexer

-- | integer parses an integer
integer :: Parser Integer
integer = Token.integer lexer

-- | whiteSpace parses whitespace
whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

nbWhiteSpace :: Parser ()
nbWhiteSpace = skipMany (satisfy isNbSpace)

isNbSpace :: Char -> Bool
isNbSpace ' ' = True
isNbSpace '\t' = True
isNbSpace _ = False

comma :: Parser ()
comma = symbol "," >> return ()

commaSep :: Parser a -> Parser [a]
commaSep a = sepBy a comma

commaSepEnd :: Parser a -> Parser [a]
commaSepEnd a = sepEndBy a comma

whileParser :: Parser Module
whileParser = whiteSpace >> moduleParser

moduleParser :: Parser Module
moduleParser =
  do
    values <- many declaration
    return $ Mod values

declaration :: Parser Declaration
declaration =
  lexeme $
    importDeclaration
      <|> constDeclaration
      <|> enumDeclaration
      <|> interfaceDeclaration
      <|> classDeclaration
      <|> implDeclaration
      <|> funcDeclaration
      <|> testDeclaration

importDeclaration :: Parser Declaration
importDeclaration =
  do
    reserved "import"
    moduleName <- stringLiteral
    specifier <- importSpecifier
    return $ Import moduleName specifier

constDeclaration :: Parser Declaration
constDeclaration =
  do
    reserved "const"
    constName <- identifier
    reservedOp "="
    value <- expr
    return $ Const constName value

enumDeclaration :: Parser Declaration
enumDeclaration =
  do
    reserved "enum"
    name <- identifier
    args <- typeArgs
    items <- braces enumItems
    return $ Enumeration name args items

interfaceDeclaration :: Parser Declaration
interfaceDeclaration =
  do
    reserved "interface"
    name <- identifier
    args <- typeArgs
    extends <- interfaceExtends
    body <- interfaceBlock
    return $ Interface name args extends body

classDeclaration :: Parser Declaration
classDeclaration =
  do
    reserved "class"
    name <- identifier
    args <- typeArgs
    pars <- optionalParams
    extends <- classExtends
    implements <- impls
    body <- classBody
    return $ Class name args pars extends implements body

implDeclaration :: Parser Declaration
implDeclaration =
  do
    reserved "impl"
    interfaceType <- typeParser
    reserved "for"
    targetType <- typeParser
    body <- classBody
    return $ Impl interfaceType targetType body

funcDeclaration :: Parser Declaration
funcDeclaration =
  do
    fnSafety <- safety
    reserved "fn"
    fnName <- identifier
    fnParams <- params
    fnReturn <- returnType
    body <- block
    return $ Func fnSafety fnName fnParams fnReturn body

testDeclaration :: Parser Declaration
testDeclaration =
  do
    reserved "test"
    testName <- identifier
    body <- testBody
    return $ Test testName body

safety :: Parser Safety
safety =
  do
    maybeUnsafe <- optionMaybe $ reserved "unsafe"
    case maybeUnsafe of
      Just _ -> return Unsafe
      Nothing -> return Safe

optionalParams :: Parser Params
optionalParams = option [] params

params :: Parser Params
params = parens commaSeparatedParams

commaSeparatedParams :: Parser Params
commaSeparatedParams = commaSepEnd param

param :: Parser Param
param =
  do
    name <- identifier
    paramType <- typeParser
    return $ Par name paramType

returnType :: Parser ReturnType
returnType = option TVoid typeParser

importSpecifier :: Parser ImportSpecifier
importSpecifier = funcImportSpecifier

funcImportSpecifier :: Parser ImportSpecifier
funcImportSpecifier =
  do
    reserved "fn"
    fnName <- identifier
    fnParams <- params
    fnReturn <- returnType
    return $ FuncImport fnName fnParams fnReturn

enumItems :: Parser [EnumItem]
enumItems = many enumItem

enumItem :: Parser EnumItem
enumItem =
  do
    itemName <- identifier
    itemParams <- optionalParams
    return $ EnumItem itemName itemParams

interfaceExtends :: Parser InterfaceExtends
interfaceExtends = option [] $ reserved "extends" >> (commaSep typeParser)

interfaceBlock :: Parser InterfaceBlock
interfaceBlock = liftM InterfaceBlock $ braces $ many methodSignature

classExtends :: Parser ClassExtends
classExtends = option TVoid $ reserved "extends" >> typeParser

impls :: Parser Implements
impls = option [] $ reserved "impl" >> (commaSep typeParser)

classBody :: Parser ClassBody
classBody = liftM ClassBody $ braces classBodyDeclarations

classBodyDeclarations :: Parser [ClassBodyDeclaration]
classBodyDeclarations = many classBodyDeclaration

classBodyDeclaration :: Parser ClassBodyDeclaration
classBodyDeclaration = try fieldDeclaration <|> staticMethodDeclaration <|> method

fieldDeclaration :: Parser ClassBodyDeclaration
fieldDeclaration =
  do
    fieldName <- identifier
    reservedOp ":="
    initializer <- expr
    return $ Field fieldName initializer

staticMethodDeclaration :: Parser ClassBodyDeclaration
staticMethodDeclaration =
  do
    reserved "static"
    Method sig body <- method
    return $ StaticMethod sig body

methodSignature :: Parser MethodSignature
methodSignature =
  do
    methodSafety <- safety
    methodName <- identifier
    methodParams <- params
    methodReturn <- returnType
    whiteSpace
    return $ MethodSignature methodSafety methodName methodParams methodReturn

method :: Parser ClassBodyDeclaration
method =
  do
    signature <- methodSignature
    body <- block
    return $ Method signature body

testBody :: Parser TestBody
testBody = liftM TestBody $ braces $ many testStmt

testStmt :: Parser TestStmt
testStmt =
  do
    reserved "it"
    description <- stringLiteral
    body <- block
    return $ ItStmt description body

block :: Parser Block
block = liftM Block $ braces $ many stmt

stmt :: Parser Stmt
stmt =
  lexeme $
    liftM IfStmt ifParser
      <|> whileStmt
      <|> returnStmt
      <|> unsafeStmt
      <|> assertStmt
      <|> liftM ExprStmt expr

ifParser :: Parser If
ifParser =
  do
    reserved "if"
    cond <- expr
    thenBlock <- block
    e <- optionMaybe elseParser
    return $ If cond thenBlock e

elseParser :: Parser Else
elseParser =
  do
    reserved "else"
    (liftM ElseIfStmt ifParser) <|> (liftM ElseStmt block)

whileStmt :: Parser Stmt
whileStmt =
  do
    reserved "while"
    cond <- expr
    whileBlock <- block
    return $ WhileStmt cond whileBlock

returnStmt :: Parser Stmt
returnStmt =
  do
    reserved "return"
    returnValue <- expr
    condition <- optionMaybe returnCondition
    return $ ReturnStmt returnValue condition

returnCondition :: Parser Expression
returnCondition =
  do
    reserved "if"
    expr

unsafeStmt :: Parser Stmt
unsafeStmt =
  do
    reserved "unsafe"
    unsafeBlock <- block
    return $ UnsafeStmt unsafeBlock

assertStmt :: Parser Stmt
assertStmt =
  do
    reserved "assert"
    cond <- expr
    message <- assertMessage cond
    return $ AssertStmt cond message

assertMessage :: Expression -> Parser String
assertMessage cond =
  do reservedOp "=>"; stringLiteral
    <|> (return $ pretty cond)

expr :: Parser Expression
expr = buildExpressionParser operators term

operators :: [[Operator Char () Expression]]
operators =
  [ [ Infix (reservedOp "." >> return (Binary Chain)) AssocLeft,
      Infix (reservedOp "?." >> return (Binary OptChain)) AssocLeft
    ],
    [Prefix (reservedOp "-" >> return (Unary Neg))],
    [Prefix (reservedOp "~" >> return (Unary BitwiseNot))],
    [Prefix (reserved "not" >> return (Unary LogicalNot))],
    [ Infix (reservedOp "*" >> return (Binary Multiply)) AssocLeft,
      Infix (reservedOp "/" >> return (Binary Divide)) AssocLeft,
      Infix (reservedOp "%" >> return (Binary Modulo)) AssocLeft
    ],
    [ Infix (reservedOp "+" >> return (Binary Add)) AssocLeft,
      Infix (reservedOp "-" >> return (Binary Subtract)) AssocLeft
    ],
    [ Infix (reservedOp ">>" >> return (Binary ShiftRight)) AssocLeft,
      Infix (reservedOp "<<" >> return (Binary ShiftLeft)) AssocLeft
    ],
    [ Infix (reservedOp "%>" >> return (Binary RotateRight)) AssocLeft,
      Infix (reservedOp "<%" >> return (Binary RotateLeft)) AssocLeft
    ],
    [ Infix (reservedOp "<" >> return (Binary LessThan)) AssocLeft,
      Infix (reservedOp "<=" >> return (Binary LessThanOrEqual)) AssocLeft,
      Infix (reservedOp ">" >> return (Binary GreaterThan)) AssocLeft,
      Infix (reservedOp ">=" >> return (Binary GreaterThanOrEqual)) AssocLeft
    ],
    [ Infix (reservedOp "==" >> return (Binary Equal)) AssocLeft,
      Infix (reservedOp "!=" >> return (Binary Unequal)) AssocLeft,
      Infix (reserved "is" >> return (Binary Is)) AssocLeft,
      Infix (reserved "as" >> return As) AssocLeft
    ],
    [ Infix (reservedOp "&" >> return (Binary BitwiseAnd)) AssocLeft,
      Infix (reservedOp "^" >> return (Binary BitwiseXor)) AssocLeft,
      Infix (reservedOp "|" >> return (Binary BitwiseOr)) AssocLeft
    ],
    [ Infix (reserved "and" >> return (Binary LogicalAnd)) AssocLeft,
      Infix (reserved "or" >> return (Binary LogicalOr)) AssocLeft
    ],
    [ Infix (reservedOp "*=" >> return (Binary AssignMultiply)) AssocLeft,
      Infix (reservedOp "/=" >> return (Binary AssignDivide)) AssocLeft,
      Infix (reservedOp "%=" >> return (Binary AssignModulo)) AssocLeft
    ],
    [ Infix (reservedOp "+=" >> return (Binary AssignAdd)) AssocLeft,
      Infix (reservedOp "-=" >> return (Binary AssignSubtract)) AssocLeft
    ],
    [ Infix (reservedOp ">>=" >> return (Binary AssignShiftRight)) AssocLeft,
      Infix (reservedOp "<<=" >> return (Binary AssignShiftLeft)) AssocLeft
    ],
    [ Infix (reservedOp "%>=" >> return (Binary AssignRotateRight)) AssocLeft,
      Infix (reservedOp "<%=" >> return (Binary AssignRotateLeft)) AssocLeft
    ],
    [ Infix (reservedOp "&=" >> return (Binary AssignBitwiseAnd)) AssocLeft,
      Infix (reservedOp "^=" >> return (Binary AssignBitwiseXor)) AssocLeft,
      Infix (reservedOp "|=" >> return (Binary AssignBitwiseOr)) AssocLeft
    ],
    [ Infix (reservedOp ":=" >> return (Binary Definition)) AssocLeft,
      Infix (reservedOp "=" >> return (Binary Assignment)) AssocLeft
    ]
  ]

term :: Parser Expression
term =
  matchExpr
    <|> try callExpr
    <|> try listAccessExpr
    <|> primaryExpression

matchExpr :: Parser Expression
matchExpr =
  do
    reserved "match"
    matchTarget <- expr
    body <- matchBody
    return $ Match matchTarget body

matchBody :: Parser MatchBody
matchBody = liftM MatchBody $ braces $ commaSepEnd matchCase

matchCase :: Parser MatchCase
matchCase =
  do
    cond <- expr
    reservedOp "=>"
    value <- expr
    return $ MatchCase cond value

primaryExpression :: Parser Expression
primaryExpression =
  parens expr
    <|> wildcard
    <|> nullLiteral
    <|> thisKeyword
    <|> liftM BooleanLiteral booleanLiteral
    <|> liftM VariableExpr identifier
    <|> numberLiteral
    <|> liftM StringLiteral stringLiteral

callExpr :: Parser Expression
callExpr =
  do
    callee <- identifier
    arg <- arguments
    return $ Call callee arg

listAccessExpr :: Parser Expression
listAccessExpr =
  do
    obj <- primaryExpression
    key <- brackets expr
    return $ ListAccess obj key

arguments :: Parser Arguments
arguments = liftM Args $ parens (commaSepEnd expr)

wildcard :: Parser Expression
wildcard = reservedOp "_" >> return Wildcard

booleanLiteral :: Parser Bool
booleanLiteral =
  (reserved "true" >> (return True))
    <|> (reserved "false" >> (return False))

stringLiteral :: Parser String
stringLiteral =
  nbLexeme
    ( do
        stringValue <- between (char '"') (char '"') (many stringChar)
        return $ foldr (maybe id (:)) "" stringValue
        <?> "string literal"
    )

stringChar :: Parser (Maybe Char)
stringChar =
  do c <- stringLetter; return (Just c)
    <|> stringEscape
    <?> "string character"

stringLetter :: Parser Char
stringLetter = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

stringEscape :: Parser (Maybe Char)
stringEscape = do
  _ <- char '\\'
  do _ <- escapeGap; return Nothing
    <|> do _ <- escapeEmpty; return Nothing
    <|> do esc <- escapeCode; return (Just esc)

escapeEmpty :: Parser Char
escapeEmpty = char '&'

escapeGap :: Parser [Char]
escapeGap = do
  many1 space

escapeCode :: Parser Char
escapeCode =
  char '"'
    <|> char '\''
    <|> char '\\'
    <|> do _ <- char 't'; return '\t'
    <|> do _ <- char 'n'; return '\n'
    <|> do _ <- char 'r'; return '\r'
    <|> do _ <- char 'e'; return '\x1b'

numberLiteral :: Parser Expression
numberLiteral = integerLiteral

integerLiteral :: Parser Expression
integerLiteral =
  do
    int <- integer

    suffix <-
      option "" $
        choice
          [ try $ symbol "XS",
            try $ symbol "UL",
            symbol "U",
            symbol "B",
            symbol "W",
            symbol "S",
            symbol "L"
          ]
    return $ NumberLiteral (suffixToPrimitiveType suffix) int

suffixToPrimitiveType :: String -> PrimitiveType
suffixToPrimitiveType "B" = TByte
suffixToPrimitiveType "XS" = TIntXS
suffixToPrimitiveType "W" = TWord
suffixToPrimitiveType "S" = TIntS
suffixToPrimitiveType "U" = TUInt
suffixToPrimitiveType "" = TInt
suffixToPrimitiveType "UL" = TUIntL
suffixToPrimitiveType "L" = TIntL
suffixToPrimitiveType _ = error "Unexpected number suffix"

nullLiteral :: Parser Expression
nullLiteral = reserved "null" >> (return NullLiteral)

thisKeyword :: Parser Expression
thisKeyword = reserved "this" >> (return ThisKeyword)

typeArgs :: Parser TypeArgs
typeArgs = option [] $ angles $ many typeParser

typeParser :: Parser Type
typeParser =
  primitiveType
    <|> listType
    <|> genericType

primitiveType :: Parser Type
primitiveType =
  do
    name <- primitiveTypeName
    return $ Primitive name

primitiveTypeName :: Parser PrimitiveType
primitiveTypeName =
  (reserved "Bool" >> return TBool)
    <|> (reserved "IntXS" >> return TIntXS)
    <|> (reserved "Byte" >> return TByte)
    <|> (reserved "IntS" >> return TIntS)
    <|> (reserved "Word" >> return TWord)
    <|> (reserved "Int" >> return TInt)
    <|> (reserved "UInt" >> return TUInt)
    <|> (reserved "IntL" >> return TIntL)
    <|> (reserved "UIntL" >> return TUIntL)
    <|> (reserved "Float" >> return TFloat)
    <|> (reserved "FloatL" >> return TFloatL)
    <|> (reserved "Char" >> return TChar)
    <|> (reserved "String" >> return TString)

listType :: Parser Type
listType = liftM List $ brackets typeParser

genericType :: Parser Type
genericType =
  do
    name <- identifier
    args <- typeArgs
    return $ Generic name args

parseString :: String -> String -> Module
parseString context str =
  case parse whileParser context str of
    Left e -> error $ "Parse error in " ++ context ++ ": " ++ (show e)
    Right r -> r

merge :: Module -> Module -> Module
merge (Mod m1) (Mod m2) = Mod (m1 ++ m2)
