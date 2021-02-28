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

      '!='            { TokenExclEq }
      '%'             { TokenRem }
      '%='            { TokenRemEq }
      '%>'            { TokenRemGt }
      '%>='           { TokenRemGtEq }
      '&'             { TokenAmp }
      '&='            { TokenAmpEq }
      '('             { TokenLParen }
      ')'             { TokenRParen }
      '*'             { TokenMul }
      '*='            { TokenMulEq }
      '+'             { TokenPlus }
      '+='            { TokenPlusEq }
      ','             { TokenComma }
      '-'             { TokenMinus }
      '-='            { TokenMinusEq }
      '.'             { TokenDot }
      '/'             { TokenDiv }
      '/='            { TokenDivEq }
      ':'             { TokenColon }
      ':='            { TokenColonEq }
      '<%'            { TokenLtRem }
      '<%='           { TokenLtRemEq }
      '<'             { TokenLt }
      '<<'            { TokenLtLt }
      '<<='           { TokenLtLtEq }
      '<='            { TokenLtEq }
      '='             { TokenEq }
      '=='            { TokenEqEq }
      '>'             { TokenGt }
      '>='            { TokenGtEq }
      '>>'            { TokenGtGt }
      '>>='           { TokenGtGtEq }
      '?'             { TokenQ }
      '?.'            { TokenQDot }
      '['             { TokenLBrack }
      ']'             { TokenRBrack }
      '^'             { TokenCaret }
      '^='            { TokenCaretEq }
      '{'             { TokenLBrace }
      '|'             { TokenBar }
      '|='            { TokenBarEq }
      '}'             { TokenRBrace }
      '~'             { TokenTilde }

      identifier      { TokenIdentifier $$ }
      int             { TokenInt $$ }
      string          { TokenString $$ }

      eos             { TokenEOS }

%left '*=' '/=' '%=' '+=' '-='  '>>=' '<<=' '%>=' '<%=' '&=' '^=' '|=' ':=' '='
%left and or
%left '&' '^' '|'
%left is as
%left '==' '!='
%left '<' '<=' '>' '>='
%left '%>' '<%'
%left '>>' '<<'
%left '+' '-'
%left '*' '/' '%'
%left NEG LNOT BNOT
%left '.' '?.'
%%

Module            :: { Module }
Module            : OptEOS Declarations                     { Module (reverse $2) }

Declarations      : Declaration                             { [$1] }
                  | Declarations Declaration                { $2 : $1 }

Declaration       : ImportDeclaration                       { $1 }
                  | ConstDeclaration                        { $1 }
                  | EnumDeclaration                         { $1 }
                  | InterfaceDeclaration                    { $1 }
                  | ImplDeclaration                         { $1 }
                  | ClassDeclaration                        { $1 }
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
InterfaceMethods      : {- empty -}                                       { [] }
                      | InterfaceMethod                                   { [$1] }
                      | InterfaceMethods InterfaceMethod                  { $2 : $1 }
InterfaceMethod       :: { InterfaceMethod }
InterfaceMethod       : identifier Arguments ReturnType EOS               { InterfaceMethod $1 $2 $3 }

ImplDeclaration       : impl Type for Type ClassBody                          { ImplDeclaration $2 $4 $5 }
ClassDeclaration      : class identifier TypeArguments OptArguments ClassExtension ClassImplementation ClassBody { ClassDeclaration $2 $3 $4 $5 $6 $7 }
ClassExtension        : {- empty -}           { [] }
                      | extends TypeList      { reverse $2 }
ClassImplementation   : {- empty -}           { [] }
                      | impl TypeList         { reverse $2 }
ClassBody             : BodyOpen ClassMethods BodyClose                       { reverse $2 }
ClassMethods          : {- empty -}                                           { [] }
                      | ClassMethod                                           { [$1] }
                      | ClassMethods ClassMethod                              { $2 : $1 }
ClassMethod           : Static Safety identifier Arguments ReturnType Block   { ClassMethod $3 $1 $2 $4 $5 $6 }

FnDeclaration     : Safety fn identifier Arguments ReturnType Block       { FnDeclaration $3 $1 $4 $5 $6 }
Block             : BodyOpen Statements BodyClose                         { reverse $2 }

Static            :: { Static }
Static            : static      { Static }
                  | {- empty -} { Instance }

Safety            :: { Safety }
Safety            : unsafe        { Unsafe }
                  | {- empty -}   { Safe }

Statements        :: { Statements }
Statements        : {- empty -}               { [] }
                  | Statement                 { [$1] }
                  | Statements                { $1 }
                  | Statements Statement      { $2 : $1 }

Statement         :: { Statement }
Statement         : VarList ':=' Expression EOS             { AssignStatement $1 $3 }
                  | IfStatement                             { IfStatement $1 }
                  | while Expression Block OptElseStatement { WhileStatement $2 $3 $4 }
                  | assert Expression AssertMessage EOS     { AssertStatement $2 $3 }
                  | return Expression ReturnCondition EOS   { ReturnStatement $2 $3 }
                  | unsafe Block                            { UnsafeStatement $2 }
                  | Expression EOS                          { ExpressionStatement $1 }

IfStatement       :: { If }
IfStatement       : if Expression Block OptElseStatement  { If $2 $3 $4 }
OptElseStatement  : {- empty -}   { Nothing }
                  | ElseStatement { Just $1 }
ElseStatement     :: { Else }
ElseStatement     : else IfStatement  { ElseIf $2 }
                  | else Block        { Else $2 }

AssertMessage     : {- empty -}               { Nothing }
                  | ':' string                { Just $2 }

ReturnCondition   : {- empty -}               { Nothing }
                  | if Expression             { Just $2 }

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
                  | Type '<' TypeArgumentList '>'           { ArgumentType $1 (reverse $3) }

FQN               :: { FQN }
FQN               : identifier                              { [$1] }
                  | FQN '.' identifier                      { $3 : $1 }

ExpressionList    :: { Expressions }
ExpressionList    : Expression                    { [$1] }
                  | ExpressionList ',' Expression { $3 : $1 }

Expression        :: { Expression }
Expression        : '(' Expression ')'            { $2 }
                  | Literal                       { LiteralExpression $1 }
                  | identifier                    { VarExpression $1 }
                  | this                          { ThisExpression }
                  | '-' Expression %prec NEG      { UnaryExpression Neg $2 }
                  | not Expression %prec LNOT     { UnaryExpression LogicalNot $2 }
                  | '~' Expression %prec BNOT     { UnaryExpression BitwiseNot $2 }
                  | Expression Params             { CallExpression $1 $2 }
                  | Expression Index              { IndexExpression $1 $2 }
                  | Expression Access             { AccessExpression $1 $2 }
                  | BinaryExpression              { $1 }

Index             :: { Expressions }
Index             : '[' ExpressionList ']' { reverse $2 }

OptAccess         :: { Maybe Access }
OptAccess         : {- empty -}         { Nothing }
                  | Access               { Just $1 }

Access            :: { Access }
Access            : '.' identifier OptAccess { Access $2 $3 }
                  | '?.' identifier OptAccess { OptAccess $2 $3 }

BinaryExpression  :: { Expression }
BinaryExpression  : Expression '*'   OptEOS Expression { BinaryExpression Multiply $1 $4 }
                  | Expression '/'   OptEOS Expression { BinaryExpression Divide $1 $4 }
                  | Expression '%'   OptEOS Expression { BinaryExpression Modulo $1 $4 }
                  | Expression '+'   OptEOS Expression { BinaryExpression Add $1 $4 }
                  | Expression '-'   OptEOS Expression { BinaryExpression Subtract $1 $4 }
                  | Expression '>>'  OptEOS Expression { BinaryExpression ShiftRight $1 $4 }
                  | Expression '<<'  OptEOS Expression { BinaryExpression ShiftLeft $1 $4 }
                  | Expression '%>'  OptEOS Expression { BinaryExpression RotateRight $1 $4 }
                  | Expression '<%'  OptEOS Expression { BinaryExpression RotateLeft $1 $4 }
                  | Expression '<'   OptEOS Expression { BinaryExpression LessThan $1 $4 }
                  | Expression '<='  OptEOS Expression { BinaryExpression LessThanOrEqual $1 $4 }
                  | Expression '>'   OptEOS Expression { BinaryExpression GreaterThan $1 $4 }
                  | Expression '>='  OptEOS Expression { BinaryExpression GreaterThanOrEqual $1 $4 }
                  | Expression '=='  OptEOS Expression { BinaryExpression Equal $1 $4 }
                  | Expression '!='  OptEOS Expression { BinaryExpression Unequal $1 $4 }
                  | Expression is    OptEOS Expression { BinaryExpression Is $1 $4 }
                  | Expression as    OptEOS Expression { BinaryExpression As $1 $4 }
                  | Expression '&'   OptEOS Expression { BinaryExpression BitwiseAnd $1 $4 }
                  | Expression '^'   OptEOS Expression { BinaryExpression BitwiseXor $1 $4 }
                  | Expression '|'   OptEOS Expression { BinaryExpression BitwiseOr $1 $4 }
                  | Expression and   OptEOS Expression { BinaryExpression LogicalAnd $1 $4 }
                  | Expression or    OptEOS Expression { BinaryExpression LogicalOr $1 $4 }
                  | Expression '*='  OptEOS Expression { BinaryExpression AssignMultiply $1 $4 }
                  | Expression '/='  OptEOS Expression { BinaryExpression AssignDivide $1 $4 }
                  | Expression '%='  OptEOS Expression { BinaryExpression AssignModulo $1 $4 }
                  | Expression '+='  OptEOS Expression { BinaryExpression AssignAdd $1 $4 }
                  | Expression '-='  OptEOS Expression { BinaryExpression AssignSubtract $1 $4 }
                  | Expression '>>=' OptEOS Expression { BinaryExpression AssignShiftRight $1 $4 }
                  | Expression '<<=' OptEOS Expression { BinaryExpression AssignShiftLeft $1 $4 }
                  | Expression '%>=' OptEOS Expression { BinaryExpression AssignRotateRight $1 $4 }
                  | Expression '<%=' OptEOS Expression { BinaryExpression AssignRotateLeft $1 $4 }
                  | Expression '&='  OptEOS Expression { BinaryExpression AssignBitwiseAnd $1 $4 }
                  | Expression '^='  OptEOS Expression { BinaryExpression AssignBitwiseXor $1 $4 }
                  | Expression '|='  OptEOS Expression { BinaryExpression AssignBitwiseOr $1 $4 }
                  | Expression '='   OptEOS Expression { BinaryExpression Assignment $1 $4 }

Params            :: { Params }
Params            : '(' ')'                   { [] }
                  | '(' ParamList ')'         { reverse $2 }
ParamList         : Expression                { [$1] }
                  | ParamList ','             { $1 }
                  | ParamList ',' Expression  { $3 : $1 }

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
