{
module MetroLang.Lang.Parser (parse) where
import MetroLang.Lang.Exception
import MetroLang.Lang.Lexer
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
