{
module MetroLang.Lang.Parser (parse) where
import Data.Char
import MetroLang.Lang.E
import MetroLang.Lang.Model
import MetroLang.Lang.Token
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
parseError tokens = failP "Parse error"

lexer :: (Token -> P a) -> P a
lexer cont [] = cont TokenEOF []
lexer cont (c:cs)
		 | isSpace c = lexer cont cs
		 | isAlpha c = lexVar cont (c:cs)
		 | isDigit c = lexNum cont (c:cs)
lexer cont ('=':cs) = cont TokenEq cs
lexer cont ('+':cs) = cont TokenPlus cs
lexer cont ('-':cs) = cont TokenMinus cs
lexer cont ('*':cs) = cont TokenTimes cs
lexer cont ('/':cs) = cont TokenDiv cs
lexer cont ('(':cs) = cont TokenOB cs
lexer cont (')':cs) = cont TokenCB cs

lexNum cont cs = cont (TokenInt (read num)) rest
		 where (num,rest) = span isDigit cs

lexVar cont cs =
	case span isAlpha cs of
		 ("let",rest) -> cont TokenLet rest
		 ("in",rest)  -> cont TokenIn rest
		 (var,rest)   -> cont (TokenVar var) rest

parse :: IO ()
parse = do
  contents <- getContents
  result <- return $ calc contents
  case result of
    Ok a      -> print a
    Failed e  -> error e
}
