module MetroLang.WebAssembly.Generator (generateString, generateFile) where

import Data.Char (isSpace)
import Data.List
import MetroLang.Bytes (toWasmStringLiteral)
import MetroLang.WebAssembly.AST

generateModule :: Module -> String
generateModule (Mod d) = wrap "module" [declarations d]

declarations :: [Declaration] -> String
declarations = indent2 . (map declaration) . sort

declaration :: Declaration -> String
declaration (Import m i s) = wrap "import" [stringLiteral m, stringLiteral i, importSpecifier s]
declaration (Memory iden i) = newline $ wrap "memory" [identifier iden, show i]
declaration (Export e s) = wrap "export" [stringLiteral e, exportSpecifier s]
declaration (Global i gt e) = wrap "global" [identifier i, globaltype gt, expr e]
declaration (Data e s) = wrap "data" [expr e, toWasmStringLiteral s]
declaration (Func iden p r s) = newline $ wrap "func" [identifier iden, params p, returnType r, indent2 $ map stmt $ s]
declaration (Start iden) = newline $ wrap "start" [identifier iden]

importSpecifier :: ImportSpecifier -> String
importSpecifier (IFunc iden p r) = wrap "func" [identifier iden, params p, returnType r]

exportSpecifier :: ExportSpecifier -> String
exportSpecifier (EMemory iden) = wrap "memory" [identifier iden]
exportSpecifier (EFunc iden) = wrap "func" [identifier iden]

stmt :: Stmt -> String
stmt (Local iden vt) = wrap "local" [identifier iden, valtype vt]
stmt (Block iden s) = wrap "block" $ [identifier iden, indent2 $ map stmt s]
stmt (Loop iden s) = wrap "loop" $ [identifier iden, indent2 $ map stmt s]
stmt (Return e) = wrap "return" [expr e]
stmt (Exp e) = expr e

expr :: Expr -> String
expr (Instr s e) = wrap s $ map expr e
expr (Method s o e) = wrap ((valtype o) ++ "." ++ s) $ map expr e
expr (MemoryInstr s o off aln e) =
  let cmd = ((valtype o) ++ "." ++ s)
      instrProps = props [("offset", off), ("align", aln)]
   in wrap (cmd ++ instrProps) $ map expr e
expr (Lit i) = show i
expr (Var iden) = identifier iden

props :: [(String, Maybe Integer)] -> String
props = concat . map prop

prop :: (String, Maybe Integer) -> String
prop (_, Nothing) = ""
prop (key, (Just value)) = " " ++ key ++ "=" ++ (show value)

params :: [Param] -> String
params = unwords . map param

param :: Param -> String
param (Par iden vt) = wrap "param" [identifier iden, valtype vt]
param (AnonymousPar vt) = wrap "param" [valtype vt]

returnType :: ReturnType -> String
returnType (Just rt) = result rt
returnType Nothing = ""

result :: Result -> String
result (Res vt) = wrap "result" [valtype vt]

globaltype :: Globaltype -> String
globaltype (Mut vt) = wrap "mut" [valtype vt]
globaltype (Imut vt) = valtype vt

valtype :: Valtype -> String
valtype I32 = "i32"
valtype I64 = "i64"
valtype F32 = "f32"
valtype F64 = "f64"

identifier :: Identifier -> String
identifier i = "$" ++ i

stringLiteral :: StringLiteral -> String
stringLiteral i = "\"" ++ i ++ "\""

indent2 :: [String] -> String
indent2 = (prefix "\n") . (indent "  ") . unlines

prefix :: String -> String -> String
prefix pref str = pref ++ str

indent :: String -> String -> String
indent identWith str = unlines $ map (prefix identWith) $ lines str

newline :: String -> String
newline str = '\n' : str

wrap :: String -> [String] -> String
wrap str content = parens $ str : content

parens :: [String] -> String
parens s = if length s == 1 then head s else "(" ++ (unwords (filter (/= "") s)) ++ ")"

trim :: String -> String
trim = reverse . (dropWhile isSpace) . reverse

trimLines :: String -> String
trimLines = unlines . (map trim) . lines

generateString :: Module -> String
generateString = trimLines . generateModule

generateFile :: String -> Module -> IO ()
generateFile file ast =
  do
    program <- return $ generateString ast
    writeFile file program
