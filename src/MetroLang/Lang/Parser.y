{
module MetroLang.Lang.Parser (parse) where
import MetroLang.Lang.Error
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
      string          { TokenString $$ }
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
      ':='            { TokenAssignment }

%%

Module            :: { Module }
Module            : Declarations                            { Module (reverse $1) }

Declarations      : Declaration                             { [$1] }
                  | Declarations Declaration                { $2 : $1 }

Declaration       : ImportDeclaration                       { $1 }
                  | ConstDeclaration                        { $1 }
                  | EnumDeclaration                         { $1 }
                  | InterfaceDeclaration                    { $1 }
                  | FnDeclaration                           { $1 }

ImportDeclaration : import FQN EOS                          { ImportDeclaration (reverse $2) }

ConstDeclaration  : const identifier '=' Expression EOS     { ConstDeclaration $2 $4 }

EnumDeclaration   : enum identifier TypeArguments EnumBody  { EnumDeclaration $2 $3 $4 }
EnumBody          : BodyOpen EnumItems BodyClose            { reverse $2 }
EnumItems         : EnumItem                                { [$1] }
                  | EnumItems EOS EnumItem                  { $3 : $1 }

EnumItem          :: { EnumItem }
EnumItem          : identifier OptArguments                 { EnumItem $1 $2 }

InterfaceDeclaration  : interface identifier TypeArguments InterfaceBody  { InterfaceDeclaration $2 $3 $4 }
InterfaceBody         : BodyOpen InterfaceMethods BodyClose               { reverse $2 }
InterfaceMethods      : InterfaceMethod                                   { [$1] }
                      | InterfaceMethods EOS InterfaceMethod              { $3 : $1 }
InterfaceMethod       :: { InterfaceMethod }
InterfaceMethod       : identifier Arguments ReturnType                   { InterfaceMethod $1 $2 $3 }

FnDeclaration     : fn identifier OptArguments ReturnType FnBody          { FnDeclaration $2 $3 $4 $5 }
FnBody            : BodyOpen Statements BodyClose                         { reverse $2 }

Statements        :: { Statements }
Statements        : {- empty -}               { [] }
                  | Statement                 { [$1] }
                  | Statements EOS            { $1 }
                  | Statements EOS Statement  { $3 : $1 }

Statement         :: { Statement }
Statement         : VarList ':=' Expression   { AssignStatement $1 $3 }
                  | Expression                { ExpressionStatement $1 }

VarList           : Vars                      { reverse $1 }
Vars              : identifier                { [$1] }
                  | Vars ',' identifier       { $3 : $1 }

OptArguments      :: { Arguments }
OptArguments      : {- empty -}                             { [] }
                  | '(' ArgumentList ')'                    { $2 }

Arguments         :: { Arguments }
Arguments         : '(' ')'                                 { [] }
                  | '(' ArgumentList ')'                    { reverse $2 }

ArgumentList      :: { Arguments }
ArgumentList      : Argument                                { [$1] }
                  | ArgumentList ','                        { $1 }
                  | ArgumentList ',' Argument               { $3 : $1 }

Argument          :: { Argument }
Argument          : identifier Type                         { Argument $1 $2 }

ReturnType        :: { ReturnType }
ReturnType        : {- empty -}                             { [] }
                  | Type                                    { [$1] }
                  | '(' TypeList ')'                        { reverse $2 }

TypeList          :: { [Type] }
TypeList          : Type                                    { [$1] }
                  | TypeList ','                            { $1 }
                  | TypeList ',' Type                       { $3 : $1 }

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
                  | '[' Type ']'                            { ArrayType $2 }

FQN               :: { FQN }
FQN               : identifier                              { [$1] }
                  | FQN '.' identifier                      { $3 : $1 }

Expression        : Literal     { LiteralExpression $1 }
                  | identifier  { VarExpression $1 }
Literal           : int         { IntLiteral $1 }
                  | string      { StringLiteral $1 }

BodyOpen          : OptEOS '{' OptEOS                       {}
BodyClose         : OptEOS '}' OptEOS                       {}

OptEOS            : {- empty -}                             {}
                  | EOS                                     {}

EOS               : eos                                     {}
                  | EOS eos                                 {}

{
parse :: IO ()
parse = do
  argv <- getArgs
  let (filePath:rest) = argv
  contents <- readFile filePath

  let result = calc contents 1 1 filePath contents
  case result of
    Ok a      -> print a
    Failed e  -> error e
}
