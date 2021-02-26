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
      and             { TokenAnd }
      as              { TokenAs }
      assert          { TokenAssert }
      class           { TokenClass }
      const           { TokenConst }
      else            { TokenElse }
      enum            { TokenEnum }
      export          { TokenExport }
      extends         { TokenExtends }
      false           { TokenFalse }
      fn              { TokenFn }
      for             { TokenFor }
      if              { TokenIf }
      it              { TokenIt }
      impl            { TokenImpl }
      import          { TokenImport }
      interface       { TokenInterface }
      is              { TokenIs }
      match           { TokenMatch }
      not             { TokenNot }
      null            { TokenNull }
      or              { TokenOr }
      return          { TokenReturn }
      static          { TokenStatic }
      test            { TokenTest }
      this            { TokenThis }
      true            { TokenTrue }
      unsafe          { TokenUnsafe }
      while           { TokenWhile }
      xor             { TokenXor }

      int             { TokenInt $$ }
      identifier      { TokenIdentifier $$ }

      '.'             { TokenDot }
      '='             { TokenEq }
      '+'             { TokenPlus }
      '-'             { TokenMinus }
      '*'             { TokenTimes }
      '/'             { TokenDiv }
      '('             { TokenOB }
      ')'             { TokenCB }

%%

Module            :: { Module }
Module            : Declarations              { Module (reverse $1) }

Declarations      : Declaration               { [$1] }
                  | Declarations Declaration  { $2 : $1 }

Declaration       : ImportDeclaration         { $1 }

ImportDeclaration : import FQN                { ImportDeclaration (reverse $2) }

FQN               :: { FQN }
FQN               : identifier                { [$1] }
                  | FQN '.' identifier        { $3 : $1 }

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
