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

      eos             { TokenEOS }
      '.'             { TokenDot }
      ','             { TokenComma }
      '='             { TokenEq }
      '+'             { TokenPlus }
      '-'             { TokenMinus }
      '*'             { TokenTimes }
      '/'             { TokenDiv }
      '('             { TokenLParen }
      ')'             { TokenRParen }
      '{'             { TokenLBrace }
      '}'             { TokenRBrace }
      '['             { TokenLBrack }
      ']'             { TokenRBrack }
      '<'             { TokenLT }
      '>'             { TokenGT }

%%

Module            :: { Module }
Module            : Declarations                            { Module (reverse $1) }

Declarations      : Declaration                             { [$1] }
                  | Declarations EOS Declaration            { $3 : $1 }

Declaration       : ImportDeclaration                       { $1 }
                  | EnumDeclaration                         { $1 }

ImportDeclaration : import FQN                              { ImportDeclaration (reverse $2) }

EnumDeclaration   : enum identifier TypeArguments EnumBody  { EnumDeclaration $2 $3 $4 }
EnumBody          : BodyOpen EnumItems BodyClose            { reverse $2 }
EnumItems         : EnumItem                                { [$1] }
                  | EnumItems EOS EnumItem                  { $3 : $1 }

EnumItem          :: { EnumItem }
EnumItem          : identifier Arguments                    { EnumItem $1 $2 }

Arguments         :: { Arguments }
Arguments         : {- empty -}                             { [] }
                  | '(' ArgumentList ')'                    { reverse $2 }

ArgumentList      :: { Arguments }
ArgumentList      : Argument                                { [$1] }
                  | ArgumentList ','                        { $1 }
                  | ArgumentList ',' Argument               { $3 : $1 }

Argument          :: { Argument }
Argument          : identifier Type                         { Argument $1 $2 }

TypeArguments     :: { TypeArguments }
TypeArguments     : {- empty -}                             { [] }
                  | '<' TypeArgumentList '>'                { reverse $2 }

TypeArgumentList  :: { TypeArguments }
TypeArgumentList  : TypeArgument                            { [$1] }
                  | TypeArgumentList ',' TypeArgument       { $3 : $1 }

TypeArgument      :: { TypeArgument }
TypeArgument      : identifier                              { TypeArgument $1 }

Type              :: { Type }
Type              : identifier                              { RefType $1 }

FQN               :: { FQN }
FQN               : identifier                              { [$1] }
                  | FQN '.' identifier                      { $3 : $1 }

BodyOpen          : OptEOS '{' OptEOS                       {}
BodyClose         : OptEOS '}' OptEOS                       {}

OptEOS            : {- empty -}                             {}
                  | EOS                                     {}

EOS               : eos                                     {}
                  | EOS eos                                 {}

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
