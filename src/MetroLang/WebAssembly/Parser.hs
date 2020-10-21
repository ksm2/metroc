module MetroLang.WebAssembly.Parser (parseString, parseFile) where

import Data.List (intercalate)
import Data.Functor.Identity
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import MetroLang.WebAssembly.AST

languageDef :: GenLanguageDef String u Identity
languageDef =
  emptyDef { Token.commentStart    = "(;"
           , Token.commentEnd      = ";)"
           , Token.commentLine     = ";;"
           , Token.identStart      = letter <|> oneOf "_"
           , Token.identLetter     = alphaNum <|> oneOf "_"
           , Token.reservedNames   = [ "(block"
                                     , "(data"
                                     , "(export"
                                     , "(func"
                                     , "(import"
                                     , "(import"
                                     , "(local"
                                     , "(memory"
                                     , "(module"
                                     , "(param"
                                     , "(result"
                                     , "(start"
                                     , "i32"
                                     , "i64"
                                     , "f32"
                                     , "f64"
                                     ]
           , Token.reservedOpNames = ["(", ")", "$", "\"", "."]
           }

lexer :: Token.GenTokenParser String u Identity
lexer = Token.makeTokenParser languageDef

strIdentifier :: Parser String
strIdentifier = Token.identifier lexer

identifier :: Parser Identifier
identifier =
  do  reservedOp "$"
      h <- strIdentifier
      t <- many dotIdentifier
      return $ intercalate "." $ h:t

dotIdentifier :: Parser String
dotIdentifier = (reservedOp ".") >> strIdentifier

reserved :: String -> Parser ()
reserved   = Token.reserved   lexer -- parses a reserved name

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer -- parses an operator

integer :: Parser Integer
integer    = Token.integer    lexer -- parses an integer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer -- parses whitespace

whileParser :: Parser Module
whileParser = whiteSpace >> moduleParser

moduleParser :: Parser Module
moduleParser =
    do  reserved "(module"
        values <- many declaration
        rparen
        return $ Mod values

declaration :: Parser Declaration
declaration =   importDecl
            <|> memoryDecl
            <|> exportDecl
            <|> dataDecl
            <|> funcDecl
            <|> startDecl

importDecl :: Parser Declaration
importDecl =
    do  reserved "(import"
        moduleString <- stringLiteral
        nameString <- stringLiteral
        specifier <- importSpecifier
        rparen
        return $ Import moduleString nameString specifier

memoryDecl :: Parser Declaration
memoryDecl =
    do  reserved "(memory"
        iden <- identifier
        idx <- integer
        rparen
        return $ Memory iden idx

exportDecl :: Parser Declaration
exportDecl =
    do  reserved "(export"
        str <- stringLiteral
        specifier <- exportSpecifier
        rparen
        return $ Export str specifier

dataDecl :: Parser Declaration
dataDecl =
    do  reserved "(data"
        idx <- expr
        dat <- stringLiteral
        rparen
        return $ Data idx dat

funcDecl :: Parser Declaration
funcDecl =
    do  reserved "(func"
        iden <- identifier
        parsedParams <- params
        body <- statement
        rparen
        return $ Func iden parsedParams body

startDecl :: Parser Declaration
startDecl =
    do  reserved "(start"
        iden <- identifier
        rparen
        return $ Start iden

importSpecifier :: Parser ImportSpecifier
importSpecifier =
    do  reserved "(func"
        iden <- identifier
        p <- params
        r <- result
        rparen
        return $ IFunc iden p r

exportSpecifier :: Parser ExportSpecifier
exportSpecifier =
    do  reserved "(memory"
        iden <- identifier
        rparen
        return $ EMemory iden

statement :: Parser Stmt
statement =
    do  list <- (many1 statement')
        -- If there's only one statement return it without using Seq.
        return $ if length list == 1 then head list else Seq list

statement' :: Parser Stmt
statement' =   localStmt
           <|> blockStmt
           <|> expStmt

localStmt :: Parser Stmt
localStmt =
    do  reserved "(local"
        iden <- identifier
        vt <- valtype
        rparen
        return $ Local iden vt

blockStmt :: Parser Stmt
blockStmt =
    do  reserved "(block"
        iden <- identifier
        s <- statement
        rparen
        return $ Block iden s

expStmt :: Parser Stmt
expStmt =
    do  e <- expr
        return $ Exp e

exprs :: Parser [Expr]
exprs = many expr

expr :: Parser Expr
expr =   litExpr
     <|> varExpr
     <|> noParensExpr
     <|> parensExpr

noParensExpr :: Parser Expr
noParensExpr =
    do  fn <- strIdentifier
        return $ Instr fn []

parensExpr :: Parser Expr
parensExpr =
    do  lparen
        e <- (methodExpr <|> instrExpr)
        rparen
        return e

instrExpr :: Parser Expr
instrExpr =
    do  fn <- strIdentifier
        e <- exprs
        return $ Instr fn e

methodExpr :: Parser Expr
methodExpr =
    do  vt <- valtype
        reservedOp "."
        fn <- strIdentifier
        e <- exprs
        return $ Method fn vt e

litExpr :: Parser Expr
litExpr =
    do  num <- integer
        return $ Lit num

varExpr :: Parser Expr
varExpr =
    do  iden <- identifier
        return $ Var iden

params :: Parser [Param]
params = many param

param :: Parser Param
param =
    do  reserved "(param"
        (par <|> anonymousPar)

par :: Parser Param
par =
    do  iden <- identifier
        vt <- valtype
        rparen
        return $ Par iden vt

anonymousPar :: Parser Param
anonymousPar =
    do  vt <- valtype
        rparen
        return $ AnonymousPar vt

result :: Parser Result
result =
    do  reserved "(result"
        vt <- valtype
        rparen
        return $ Res vt

stringLiteral :: Parser StringLiteral
stringLiteral =
    do  _ <- oneOf "\""
        stringValue <- many (noneOf "\"")
        _ <- oneOf "\""
        whiteSpace
        return stringValue

valtype :: Parser Valtype
valtype =   (reserved "i32" >> return I32)
        <|> (reserved "i64" >> return I64)
        <|> (reserved "f32" >> return F32)
        <|> (reserved "f64" >> return F64)

lparen :: Parser ()
lparen = reservedOp "("

rparen :: Parser ()
rparen = reservedOp ")"

parseString :: String -> Module
parseString str =
  case parse whileParser "" str of
    Left e  -> error $ show e
    Right r -> r

parseFile :: String -> IO Module
parseFile file =
  do program  <- readFile file
     case parse whileParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r