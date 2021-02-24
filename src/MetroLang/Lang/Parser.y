{
module MetroLang.Lang.Parser (parse) where
import Data.Char
import MetroLang.Lang.Exception
import MetroLang.Lang.Model
import MetroLang.Lang.Token
import System.Environment
}

%name calc
%tokentype { Token }
%monad { P } { thenP } { returnP }
%lexer { lexer } { TokenEOF }
%error { parseError }

%token
      let             { TokenLet }
      in              { TokenIn }
      int             { TokenInt $$ }
      var             { TokenVar $$ }
      '='             { TokenEq }
      '+'             { TokenPlus }
      '-'             { TokenMinus }
      '*'             { TokenTimes }
      '/'             { TokenDiv }
      '('             { TokenOB }
      ')'             { TokenCB }

%%

Exp   : let var '=' Exp in Exp  { Let $2 $4 $6 }
      | Exp1                    { Exp1 $1 }

Exp1  : Exp1 '+' Term           { Plus $1 $3 }
      | Exp1 '-' Term           { Minus $1 $3 }
      | Term                    { Term $1 }

Term  : Term '*' Factor         { Times $1 $3 }
      | Term '/' Factor         { Div $1 $3 }
      | Factor                  { Factor $1 }

Factor
      : int                     { Int $1 }
      | var                     { Var $1 }
      | '(' Exp ')'             { Brack $2 }

{
parseError :: Token -> P a
parseError tokens = getLineNo `thenP` \line ->
                    getColNo `thenP` \col ->
                    getInputFile `thenP` \input ->
                    failP ("Parse error in " ++ input ++ " on line " ++ show line ++ ", column " ++ show col)

lexer :: (Token -> P a) -> P a
lexer cont [] = cont TokenEOF []
lexer cont ('\n':cs) = \col line -> lexer cont cs 0 (line + 1)
lexer cont (c:cs)
		 | isSpace c = \col -> lexer cont cs (col + 1)
		 | isAlpha c = lexVar cont (c:cs)
		 | isDigit c = lexNum cont (c:cs)
lexer cont ('=':cs) = \col -> cont TokenEq cs (col + 1)
lexer cont ('+':cs) = \col -> cont TokenPlus cs (col + 1)
lexer cont ('-':cs) = \col -> cont TokenMinus cs (col + 1)
lexer cont ('*':cs) = \col -> cont TokenTimes cs (col + 1)
lexer cont ('/':cs) = \col -> cont TokenDiv cs (col + 1)
lexer cont ('(':cs) = \col -> cont TokenOB cs (col + 1)
lexer cont (')':cs) = \col -> cont TokenCB cs (col + 1)

lexNum cont cs = \col -> cont (TokenInt (read num)) rest (col + (length num))
		 where (num,rest) = span isDigit cs

lexVar cont cs =
	case span isAlpha cs of
		 ("let",rest) -> \col -> cont TokenLet rest (col + 3)
		 ("in",rest)  -> \col -> cont TokenIn rest (col + 2)
		 (var,rest)   -> \col -> cont (TokenVar var) rest (col + (length var))

parse :: IO ()
parse = do
  argv <- getArgs
  let (filePath:rest) = argv
  contents <- readFile filePath

  let result = calc contents 0 1 filePath
  case result of
    Ok a      -> print a
    Failed e  -> error e
}
