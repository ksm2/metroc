module MetroLang.WebAssembly.Parser (parseString, parseFile, merge) where

import Data.Functor.Identity
import Data.List (intercalate)
import MetroLang.Bytes
import MetroLang.WebAssembly.AST
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDef :: GenLanguageDef String u Identity
languageDef =
  emptyDef
    { Token.commentStart = "(;",
      Token.commentEnd = ";)",
      Token.commentLine = ";;",
      Token.identStart = letter <|> oneOf "_",
      Token.identLetter = alphaNum <|> oneOf "_",
      Token.reservedNames =
        [ "(block",
          "(data",
          "(export",
          "(func",
          "(import",
          "(import",
          "(local",
          "(loop",
          "(memory",
          "(module",
          "(mut",
          "(param",
          "(result",
          "(return",
          "select",
          "(start",
          "i32",
          "i64",
          "f32",
          "f64"
        ],
      Token.reservedOpNames = ["(", ")", "$", "\"", ".", "="]
    }

lexer :: Token.GenTokenParser String u Identity
lexer = Token.makeTokenParser languageDef

strIdentifier :: Parser String
strIdentifier = Token.identifier lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

identifier :: Parser Identifier
identifier =
  do
    reservedOp "$"
    h <- strIdentifier
    t <- many dotIdentifier
    return $ intercalate "." $ h : t

dotIdentifier :: Parser String
dotIdentifier = reservedOp "." >> strIdentifier

reserved :: String -> Parser ()
reserved = Token.reserved lexer -- parses a reserved name

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer -- parses an operator

integer :: Parser Integer
integer = Token.integer lexer -- parses an integer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer -- parses whitespace

whileParser :: Parser Module
whileParser = whiteSpace >> moduleParser

moduleParser :: Parser Module
moduleParser =
  do
    reserved "(module"
    values <- many declaration
    rparen
    return $ Mod values

declaration :: Parser Declaration
declaration =
  importDecl
    <|> memoryDecl
    <|> exportDecl
    <|> globalDecl
    <|> dataDecl
    <|> funcDecl
    <|> startDecl

importDecl :: Parser Declaration
importDecl =
  do
    reserved "(import"
    moduleString <- stringLiteral
    nameString <- stringLiteral
    specifier <- importSpecifier
    rparen
    return $ Import moduleString nameString specifier

memoryDecl :: Parser Declaration
memoryDecl =
  do
    reserved "(memory"
    iden <- identifier
    idx <- integer
    rparen
    return $ Memory iden idx

exportDecl :: Parser Declaration
exportDecl =
  do
    reserved "(export"
    str <- stringLiteral
    specifier <- exportSpecifier
    rparen
    return $ Export str specifier

globalDecl :: Parser Declaration
globalDecl =
  do
    reserved "(global"
    globalIdentifier <- identifier
    vt <- globaltype
    initialValue <- expr
    rparen
    return $ Global globalIdentifier vt initialValue

dataDecl :: Parser Declaration
dataDecl =
  do
    reserved "(data"
    idx <- expr
    dat <- bytes
    rparen
    return $ Data idx dat

funcDecl :: Parser Declaration
funcDecl =
  do
    reserved "(func"
    iden <- identifier
    parsedParams <- params
    fnReturn <- returnType
    body <- many expr
    rparen
    return $ Func iden Nothing parsedParams fnReturn body

startDecl :: Parser Declaration
startDecl =
  do
    reserved "(start"
    iden <- identifier
    rparen
    return $ Start iden

importSpecifier :: Parser ImportSpecifier
importSpecifier =
  do
    reserved "(func"
    iden <- identifier
    p <- params
    fnReturn <- returnType
    rparen
    return $ IFunc iden p fnReturn

exportSpecifier :: Parser ExportSpecifier
exportSpecifier = memoryExportSpecifier <|> funcExportSpecifier

memoryExportSpecifier :: Parser ExportSpecifier
memoryExportSpecifier =
  do
    reserved "(memory"
    iden <- identifier
    rparen
    return $ EMemory iden

funcExportSpecifier :: Parser ExportSpecifier
funcExportSpecifier =
  do
    reserved "(func"
    iden <- identifier
    rparen
    return $ EFunc iden

exprs :: Parser [Expr]
exprs = many expr

expr :: Parser Expr
expr =
  localExpr
    <|> blockExpr
    <|> loopExpr
    <|> returnExpr
    <|> litExpr
    <|> varExpr
    <|> noParensExpr
    <|> parensExpr

localExpr :: Parser Expr
localExpr =
  do
    reserved "(local"
    iden <- identifier
    vt <- valtype
    rparen
    return $ Local iden vt

blockExpr :: Parser Expr
blockExpr =
  do
    reserved "(block"
    iden <- identifier
    rt <- returnType
    s <- many1 expr
    rparen
    return $ Block iden rt s

loopExpr :: Parser Expr
loopExpr =
  do
    reserved "(loop"
    iden <- identifier
    s <- many1 expr
    rparen
    return $ Loop iden s

returnExpr :: Parser Expr
returnExpr =
  do
    reserved "(return"
    e <- expr
    rparen
    return $ Return e

noParensExpr :: Parser Expr
noParensExpr =
  do
    fn <- strIdentifier
    return $ Instr fn []

parensExpr :: Parser Expr
parensExpr = parens instruction

instruction :: Parser Expr
instruction = selectExpr <|> try memoryInstrExpr <|> methodExpr <|> instrExpr

memoryInstrExpr :: Parser Expr
memoryInstrExpr =
  do
    vt <- valtype
    reservedOp "."
    fn <- memoryOp
    props <- instrProps
    let offset = props ! "offset"
    let align = props ! "align"
    MemoryInstr fn vt offset align <$> exprs

memoryOp :: Parser String
memoryOp = loadOp <|> storeOp

loadOp :: Parser String
loadOp =
  do
    prefix <- string "load"
    suffix <- option "" signedBitWidth
    whiteSpace
    return $ prefix ++ suffix

storeOp :: Parser String
storeOp =
  do
    prefix <- string "store"
    suffix <- option "" bitWidth
    whiteSpace
    return $ prefix ++ suffix

signedBitWidth :: Parser String
signedBitWidth =
  do
    bw <- bitWidth
    _ <- char '_'
    sign <- oneOf "su"
    return $ bw ++ "_" ++ [sign]

bitWidth :: Parser String
bitWidth = string "8" <|> string "16" <|> string "32"

(!) :: [(String, Integer)] -> String -> Maybe Integer
(!) [] _ = Nothing
(!) ((mapKey, mapValue) : mapTail) key =
  if mapKey == key then Just mapValue else mapTail ! key

instrProps :: Parser [(String, Integer)]
instrProps = many instrProp

instrProp :: Parser (String, Integer)
instrProp =
  do
    key <- try strIdentifier
    reservedOp "="
    value <- integer
    return (key, value)

instrExpr :: Parser Expr
instrExpr =
  do
    fn <- strIdentifier
    Instr fn <$> exprs

methodExpr :: Parser Expr
methodExpr =
  do
    vt <- valtype
    reservedOp "."
    fn <- strIdentifier
    Method fn vt <$> exprs

selectExpr :: Parser Expr
selectExpr =
  do
    reserved "select"
    left <- expr
    right <- expr
    Select left right <$> expr

litExpr :: Parser Expr
litExpr =
    Lit <$> integer

varExpr :: Parser Expr
varExpr =
    Var <$> identifier

params :: Parser [Param]
params = many param

param :: Parser Param
param =
  do
    reserved "(param"
    par <|> anonymousPar

par :: Parser Param
par =
  do
    iden <- identifier
    vt <- valtype
    rparen
    return $ Par iden vt

anonymousPar :: Parser Param
anonymousPar =
  do
    vt <- valtype
    rparen
    return $ AnonymousPar vt

returnType :: Parser ReturnType
returnType = optionMaybe result

result :: Parser Result
result =
  do
    reserved "(result"
    vt <- valtype
    rparen
    return $ Res vt

bytes :: Parser Bytes
bytes =
    fromString <$> stringLiteral

stringLiteral :: Parser StringLiteral
stringLiteral =
  do
    _ <- oneOf "\""
    stringValue <- many (noneOf "\"")
    _ <- oneOf "\""
    whiteSpace
    return stringValue

globaltype :: Parser Globaltype
globaltype = mutableGlobal <|> fmap Imut valtype

mutableGlobal :: Parser Globaltype
mutableGlobal =
  do
    reserved "(mut"
    x <- valtype
    rparen
    return $ Mut x

valtype :: Parser Valtype
valtype =
  (reserved "i32" >> return I32)
    <|> (reserved "i64" >> return I64)
    <|> (reserved "f32" >> return F32)
    <|> (reserved "f64" >> return F64)

rparen :: Parser ()
rparen = reservedOp ")"

parseString :: String -> Module
parseString str =
  case parse whileParser "" str of
    Left e -> error $ show e
    Right r -> r

parseFile :: String -> IO Module
parseFile file =
  do
    program <- readFile file
    case parse whileParser "" program of
      Left e -> print e >> fail "parse error"
      Right r -> return r

merge :: Module -> Module -> Module
merge (Mod d1) (Mod d2) = Mod (d1 ++ d2)
