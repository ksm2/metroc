module MetroLang.WebAssembly.Generator (generateString, generateFile) where

import Data.List
import MetroLang.WebAssembly.AST

generateModule :: Module -> String
generateModule (Mod d) = wrap "module" [declarations d]

declarations :: [Declaration] -> String
declarations = spaces . map declaration

declaration :: Declaration -> String
declaration (Import m i s) = wrap "import" [stringLiteral m, stringLiteral i, importSpecifier s]
declaration (Memory iden i) = wrap "memory" [identifier iden, show i]
declaration (Export e s) = wrap "export" [stringLiteral e, exportSpecifier s]
declaration (Data e s) = wrap "data" [expr e, stringLiteral s]
declaration (Func iden p s) = wrap "func" [identifier iden, params p, stmt s]
declaration (Start iden) = wrap "start" [identifier iden]

importSpecifier :: ImportSpecifier -> String
importSpecifier (IFunc iden p r) = wrap "func" [identifier iden, params p, result r]

exportSpecifier :: ExportSpecifier -> String
exportSpecifier (EMemory iden) = wrap "memory" [identifier iden]

stmt :: Stmt -> String
stmt (Local iden vt) = wrap "local" [identifier iden, valtype vt]
stmt (Block iden s) = wrap "block" [identifier iden, stmt s]
stmt (Exp e) = expr e
stmt (Seq s) = (spaces . map stmt) s

expr :: Expr -> String
expr (Instr s e) = wrap s $ map expr e
expr (Method s o e) = wrap (s ++ "." ++ (valtype o)) $ map expr e
expr (Lit i) = show i
expr (Var iden) = identifier iden

params :: [Param] -> String
params = spaces . map param

param :: Param -> String
param (Par iden vt) = wrap "param" [identifier iden, valtype vt]
param (AnonymousPar vt) = wrap "param" [valtype vt]

result :: Result -> String
result (Res vt) = wrap "result" [valtype vt]

valtype :: Valtype -> String
valtype I32 = "i32"
valtype I64 = "i64"
valtype F32 = "f32"
valtype F64 = "f64"

identifier :: Identifier -> String
identifier i = "$" ++ i

stringLiteral :: StringLiteral -> String
stringLiteral i = "\"" ++ i ++ "\""

spaces :: [String] -> String
spaces = intercalate " "

wrap :: String -> [String] -> String
wrap str content = "(" ++ str ++ " " ++ (spaces content) ++ ")"

generateString :: Module -> String
generateString = generateModule

generateFile :: String -> Module -> IO ()
generateFile file ast =
    do  program <- return $ generateString ast
        writeFile file program
