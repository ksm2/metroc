{
module MetroLang.Lang.Parser (parse, merge) where
import MetroLang.Lang.Error
import MetroLang.Lang.Exception
import MetroLang.Lang.Lexer
import MetroLang.Lang.Model
import MetroLang.Lang.Token
}

%name calc
%tokentype { Token }
%monad { P } { thenP } { returnP }
%lexer { lexer } { TokenEOF }
%error { parseError }

%token
      and               { TokenAnd }
      as                { TokenAs }
      assert            { TokenAssert }
      class             { TokenClass }
      const             { TokenConst }
      else              { TokenElse }
      enum              { TokenEnum }
      export            { TokenExport }
      extends           { TokenExtends }
      external          { TokenExternal }
      false             { TokenFalse }
      fn                { TokenFn }
      for               { TokenFor }
      hide              { TokenHide }
      if                { TokenIf }
      it                { TokenIt }
      impl              { TokenImpl }
      import            { TokenImport }
      interface         { TokenInterface }
      is                { TokenIs }
      let               { TokenLet }
      match             { TokenMatch }
      not               { TokenNot }
      null              { TokenNull }
      or                { TokenOr }
      return            { TokenReturn }
      static            { TokenStatic }
      test              { TokenTest }
      this              { TokenThis }
      true              { TokenTrue }
      unsafe            { TokenUnsafe }
      while             { TokenWhile }
      xor               { TokenXor }

      Bool      { TokenTBool }
      IntXS     { TokenTIntXS }
      Byte      { TokenTByte }
      IntS      { TokenTIntS }
      Word      { TokenTWord }
      Int       { TokenTInt }
      UInt      { TokenTUInt }
      IntL      { TokenTIntL }
      UIntL     { TokenTUIntL }
      Float     { TokenTFloat }
      FloatL    { TokenTFloatL }
      Char      { TokenTChar }
      String    { TokenTString }

      '!='      { TokenExclEq }
      '%'       { TokenRem }
      '%='      { TokenRemEq }
      '%>'      { TokenRemGt }
      '%>='     { TokenRemGtEq }
      '&'       { TokenAmp }
      '&='      { TokenAmpEq }
      '('       { TokenLParen }
      ')'       { TokenRParen }
      '*'       { TokenMul }
      '*='      { TokenMulEq }
      '+'       { TokenPlus }
      '+='      { TokenPlusEq }
      ','       { TokenComma }
      '-'       { TokenMinus }
      '-='      { TokenMinusEq }
      '.'       { TokenDot }
      '/'       { TokenDiv }
      '/='      { TokenDivEq }
      ':'       { TokenColon }
      ':='      { TokenColonEq }
      '<%'      { TokenLtRem }
      '<%='     { TokenLtRemEq }
      '<'       { TokenLt }
      '<<'      { TokenLtLt }
      '<<='     { TokenLtLtEq }
      '<='      { TokenLtEq }
      '='       { TokenEq }
      '=='      { TokenEqEq }
      '=>'      { TokenEqGt }
      '>'       { TokenGt }
      '>='      { TokenGtEq }
      '>>'      { TokenGtGt }
      '>>='     { TokenGtGtEq }
      '?'       { TokenQ }
      '?.'      { TokenQDot }
      '['       { TokenLBrack }
      ']'       { TokenRBrack }
      '^'       { TokenCaret }
      '^='      { TokenCaretEq }
      '_'       { TokenUnderscore }
      '{'       { TokenLBrace }
      '|'       { TokenBar }
      '|='      { TokenBarEq }
      '}'       { TokenRBrace }
      '~'       { TokenTilde }

      id        { TokenIdentifier $$ }
      int       { TokenInt $$ }
      uint      { TokenUInt $$ }
      byte      { TokenByte $$ }
      string    { TokenString $$ }

      eos       { TokenEOS }

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

Module                  :: { Module }
Module                  : OptEOS Declarations                           { Module (reverse $2) }

Declarations            : Declaration                                   { [$1] }
                        | Declarations Declaration                      { $2 : $1 }

Declaration             :: { Declaration }
Declaration             : ImportDeclaration                             { $1 }
                        | ExportDeclaration                             { $1 }
                        | TestDeclaration                               { $1 }
                        | HideDeclaration                               { $1 }
                        | HideableDeclaration                           { $1 }

HideableDeclaration     :: { Declaration }
HideableDeclaration     : ImplDeclaration                               { $1 }
                        | ExportableDeclaration                         { $1 }

ExportableDeclaration   :: { Declaration }
ExportableDeclaration   : ConstDeclaration                              { $1 }
                        | ExternalDeclaration                           { $1 }
                        | EnumDeclaration                               { $1 }
                        | InterfaceDeclaration                          { $1 }
                        | ClassDeclaration                              { $1 }
                        | FnDeclaration                                 { $1 }

ImportDeclaration       : import FQN EOS                                { ImportDeclaration (reverse $2) }

ExportDeclaration       : export ExportableDeclaration                  { ExportDeclaration $2 }

TestDeclaration         : test id TestBlock                             { TestDeclaration $2 $3 }
TestBlock               : BodyOpen TestStatements BodyClose             { reverse $2 }
TestStatements          :: { [TestStatement] }
TestStatements          : {- empty -}                                   { [] }
                        | TestStatement                                 { [$1] }
                        | TestStatements TestStatement                  { $2 : $1 }
TestStatement           :: { TestStatement }
TestStatement           : it string Block                               { TestStatement $2 $3 }

HideDeclaration         : hide HideableDeclaration                      { HideDeclaration $2 }

ConstDeclaration        : const id '=' Expression EOS                   { ConstDeclaration $2 $4 }

ExternalDeclaration     : external string External EOS                  { ExternalDeclaration $2 $3 }
External                :: { External }
External                : fn id Params ReturnType                       { FnExternal $2 $3 $4 }

EnumDeclaration         : enum id TypeArguments EnumBody                { EnumDeclaration $2 $3 $4 }
EnumBody                : BodyOpen EnumItems BodyClose                  { reverse $2 }
EnumItems               : EnumItem                                      { [$1] }
                        | EnumItems EOS EnumItem                        { $3 : $1 }

EnumItem                :: { EnumItem }
EnumItem                : id OptParams                                  { EnumItem $1 $2 }

InterfaceDeclaration    : interface id TypeArguments InterfaceBody      { InterfaceDeclaration $2 $3 $4 }
InterfaceBody           : BodyOpen InterfaceMethods BodyClose           { reverse $2 }
InterfaceMethods        : {- empty -}                                   { [] }
                        | InterfaceMethod                               { [$1] }
                        | InterfaceMethods InterfaceMethod              { $2 : $1 }
InterfaceMethod         :: { InterfaceMethod }
InterfaceMethod         : id Arguments ReturnType EOS                   { InterfaceMethod $1 $2 $3 }

ImplDeclaration         : impl Type for Type ClassBody                  { ImplDeclaration $2 $4 $5 }

ClassDeclaration        : class id TypeArguments OptParams ClassExtension ClassImplementation ClassBody { ClassDeclaration $2 $3 $4 $5 $6 $7 }
ClassExtension          : {- empty -}                                   { [] }
                        | extends TypeList                              { reverse $2 }
ClassImplementation     : {- empty -}                                   { [] }
                        | impl TypeList                                 { reverse $2 }
ClassBody               :: { ClassBody }
ClassBody               : BodyOpen ClassElements BodyClose              { reverse $2 }
ClassElements           : {- empty -}                                   { [] }
                        | ClassElement                                  { [$1] }
                        | ClassElements ClassElement                    { $2 : $1 }
ClassElement            : static id ':=' Expression EOS                 { StaticField $2 $4 }
                        | static MethodSignature Block                  { StaticMethod $2 $3 }
                        | id ':=' Expression EOS                        { Field $1 $3 }
                        | MethodSignature Block                         { Method $1 $2 }

MethodSignature         :: { MethodSignature }
MethodSignature         : id Params ReturnType                          { MethodSignature Safe $1 $2 $3 }
                        | unsafe id Params ReturnType                   { MethodSignature Unsafe $2 $3 $4 }

FnDeclaration           : Safety fn id Params ReturnType Block          { FnDeclaration $3 $1 $4 $5 $6 }
Block                   :: { Block }
Block                   : BodyOpen Statements BodyClose                 { reverse $2 }

Safety                  :: { Safety }
Safety                  : unsafe                                        { Unsafe }
                        | {- empty -}                                   { Safe }

Statements              :: { [Statement] }
Statements              : {- empty -}                                   { [] }
                        | Statement                                     { [$1] }
                        | Statements                                    { $1 }
                        | Statements Statement                          { $2 : $1 }

Statement               :: { Statement }
Statement               : id ':=' Expression EOS                        { AssignStatement $1 $3 }
                        | IfStatement                                   { IfStatement $1 }
                        | while Expression Block                        { WhileStatement $2 $3 }
                        | assert Expression AssertMessage EOS           { AssertStatement $2 $3 }
                        | return Expression ReturnCondition EOS         { ReturnStatement $2 $3 }
                        | unsafe Block                                  { UnsafeStatement $2 }
                        | Expression EOS                                { ExpressionStatement $1 }

IfStatement             :: { If }
IfStatement             : if Expression Block OptElse                   { If $2 $3 $4 }

OptElse                 : {- empty -}                                   { Nothing }
                        | ElseStatement                                 { Just $1 }
ElseStatement           :: { Else }
ElseStatement           : else IfStatement                              { ElseIf $2 }
                        | else Block                                    { Else $2 }

AssertMessage           :: { String }
AssertMessage           : ':' string                                    { $2 }

ReturnCondition         : {- empty -}                                   { Nothing }
                        | if Expression                                 { Just $2 }

VarList                 : Vars                                          { reverse $1 }
Vars                    : id                                            { [$1] }
                        | Vars ',' id                                   { $3 : $1 }

OptParams               :: { Params }
OptParams               : {- empty -}                                   { [] }
                        | '(' ParamList ')'                             { $2 }

Params                  :: { Params }
Params                  : '(' ')'                                       { [] }
                        | '(' ParamList ')'                             { reverse $2 }

ParamList               :: { Params }
ParamList               : Param                                         { [$1] }
                        | ParamList ','                                 { $1 }
                        | ParamList ',' Param                           { $3 : $1 }

Param                   :: { Param }
Param                   : id Type                                       { Param $1 $2 }

ReturnType              :: { ReturnType }
ReturnType              : {- empty -}                                   { VoidType }
                        | Type                                          { $1 }

TypeList                :: { [Type] }
TypeList                : Type                                          { [$1] }
                        | TypeList ','                                  { $1 }
                        | TypeList ',' Type                             { $3 : $1 }

TypeArguments           :: { TypeArguments }
TypeArguments           : {- empty -}                                   { [] }
                        | '<' TypeArgumentList '>'                      { reverse $2 }

TypeArgumentList        :: { TypeArguments }
TypeArgumentList        : TypeArgument                                  { [$1] }
                        | TypeArgumentList ',' TypeArgument             { $3 : $1 }

TypeArgument            :: { TypeArgument }
TypeArgument            : id                                            { TypeArgument $1 }

Type                    :: { Type }
Type                    : id                                            { RefType $1 }
                        | PrimitiveType                                 { PrimitiveType $1 }
                        | '[' Type ']'                                  { ArrayType $2 }
                        | Type '<' TypeArgumentList '>'                 { GenericType $1 (reverse $3) }

PrimitiveType           :: { PrimitiveType }
PrimitiveType           : Bool                                          { TBool }
                        | IntXS                                         { TIntXS }
                        | Byte                                          { TByte }
                        | IntS                                          { TIntS }
                        | Word                                          { TWord }
                        | Int                                           { TInt }
                        | UInt                                          { TUInt }
                        | IntL                                          { TIntL }
                        | UIntL                                         { TUIntL }
                        | Float                                         { TFloat }
                        | FloatL                                        { TFloatL }
                        | Char                                          { TChar }
                        | String                                        { TString }

FQN                     :: { FQN }
FQN                     : id                                            { [$1] }
                        | FQN '.' id                                    { $3 : $1 }

ExpressionList          :: { Expressions }
ExpressionList          : Expression                                    { [$1] }
                        | ExpressionList ',' Expression                 { $3 : $1 }

Expression              :: { Expression }
Expression              : '(' Expression ')'                            { $2 }
                        | Literal                                       { LiteralExpression $1 }
                        | id                                            { VarExpression $1 }
                        | this                                          { ThisExpression }
                        | null                                          { NullExpression }
                        | Expression as Type                            { CastExpression $1 $3 }
                        | id Arguments                                  { CallExpression $1 $2 }
                        | Expression Index                              { IndexExpression $1 $2 }
                        | Expression '.' id Arguments                   { MethodCallExpression $1 $3 $4 }
                        | Expression '.' id                             { AccessExpression $1 $3 }
                        | Type                                          { TypeExpression $1 }
                        | MatchExpression                               { $1 }
                        | UnaryExpression                               { $1 }
                        | BinaryExpression                              { $1 }

Index                   :: { Expression }
Index                   : '[' Expression ']'                            { $2 }

MatchExpression         :: { Expression }
MatchExpression         : match Expression MatchBody                    { MatchExpression $2 $3 }
MatchBody               : BodyOpen MatchRules BodyClose                 { reverse $2 }
MatchRules              : MatchRule                                     { [$1] }
                        | MatchRules EOS MatchRule                      { $3 : $1 }
MatchRule               : MatchCondition '=>' Expression                { MatchRule $1 $3 }
MatchCondition          : '_'                                           { MatchWildcard }
                        | Literal                                       { MatchPattern $1 }

UnaryExpression         :: { Expression }
UnaryExpression         : '-' Expression %prec NEG                      { UnaryExpression Neg $2 }
                        | not Expression %prec LNOT                     { UnaryExpression LogicalNot $2 }
                        | '~' Expression %prec BNOT                     { UnaryExpression BitwiseNot $2 }

BinaryExpression        :: { Expression }
BinaryExpression        : Expression '*'   OptEOS Expression            { BinaryExpression Multiply $1 $4 }
                        | Expression '/'   OptEOS Expression            { BinaryExpression Divide $1 $4 }
                        | Expression '%'   OptEOS Expression            { BinaryExpression Modulo $1 $4 }
                        | Expression '+'   OptEOS Expression            { BinaryExpression Add $1 $4 }
                        | Expression '-'   OptEOS Expression            { BinaryExpression Subtract $1 $4 }
                        | Expression '>>'  OptEOS Expression            { BinaryExpression ShiftRight $1 $4 }
                        | Expression '<<'  OptEOS Expression            { BinaryExpression ShiftLeft $1 $4 }
                        | Expression '%>'  OptEOS Expression            { BinaryExpression RotateRight $1 $4 }
                        | Expression '<%'  OptEOS Expression            { BinaryExpression RotateLeft $1 $4 }
                        | Expression '<'   OptEOS Expression            { BinaryExpression LessThan $1 $4 }
                        | Expression '<='  OptEOS Expression            { BinaryExpression LessThanOrEqual $1 $4 }
                        | Expression '>'   OptEOS Expression            { BinaryExpression GreaterThan $1 $4 }
                        | Expression '>='  OptEOS Expression            { BinaryExpression GreaterThanOrEqual $1 $4 }
                        | Expression '=='  OptEOS Expression            { BinaryExpression Equal $1 $4 }
                        | Expression '!='  OptEOS Expression            { BinaryExpression Unequal $1 $4 }
                        | Expression is    OptEOS Expression            { BinaryExpression Is $1 $4 }
                        | Expression '&'   OptEOS Expression            { BinaryExpression BitwiseAnd $1 $4 }
                        | Expression '^'   OptEOS Expression            { BinaryExpression BitwiseXor $1 $4 }
                        | Expression '|'   OptEOS Expression            { BinaryExpression BitwiseOr $1 $4 }
                        | Expression and   OptEOS Expression            { BinaryExpression LogicalAnd $1 $4 }
                        | Expression or    OptEOS Expression            { BinaryExpression LogicalOr $1 $4 }
                        | Expression '*='  OptEOS Expression            { BinaryExpression AssignMultiply $1 $4 }
                        | Expression '/='  OptEOS Expression            { BinaryExpression AssignDivide $1 $4 }
                        | Expression '%='  OptEOS Expression            { BinaryExpression AssignModulo $1 $4 }
                        | Expression '+='  OptEOS Expression            { BinaryExpression AssignAdd $1 $4 }
                        | Expression '-='  OptEOS Expression            { BinaryExpression AssignSubtract $1 $4 }
                        | Expression '>>=' OptEOS Expression            { BinaryExpression AssignShiftRight $1 $4 }
                        | Expression '<<=' OptEOS Expression            { BinaryExpression AssignShiftLeft $1 $4 }
                        | Expression '%>=' OptEOS Expression            { BinaryExpression AssignRotateRight $1 $4 }
                        | Expression '<%=' OptEOS Expression            { BinaryExpression AssignRotateLeft $1 $4 }
                        | Expression '&='  OptEOS Expression            { BinaryExpression AssignBitwiseAnd $1 $4 }
                        | Expression '^='  OptEOS Expression            { BinaryExpression AssignBitwiseXor $1 $4 }
                        | Expression '|='  OptEOS Expression            { BinaryExpression AssignBitwiseOr $1 $4 }
                        | Expression '='   OptEOS Expression            { BinaryExpression Assignment $1 $4 }

Arguments               :: { Arguments }
Arguments               : '(' ')'                                       { [] }
                        | '(' ArgumentList ')'                          { reverse $2 }
ArgumentList            : Expression                                    { [$1] }
                        | ArgumentList ','                              { $1 }
                        | ArgumentList ',' Expression                   { $3 : $1 }

Literal                 :: { Literal }
Literal                 : int                                           { IntLiteral $1 }
                        | uint                                          { UIntLiteral $1 }
                        | byte                                          { ByteLiteral $1 }
                        | string                                        { StringLiteral $1 }
                        | true                                          { BoolLiteral True }
                        | false                                         { BoolLiteral False }

BodyOpen                : OptEOS '{' OptEOS                             {}
BodyClose               : OptEOS '}' OptEOS                             {}

OptEOS                  : {- empty -}                                   {}
                        | EOS                                           {}

EOS                     : eos                                           {}
                        | EOS eos                                       {}

{
parse :: String -> String -> Module
parse filePath contents =
  let result = calc contents 1 1 filePath contents
  in case result of
    Ok a      -> a
    Failed e  -> error e

merge :: Module -> Module -> Module
merge (Module m1) (Module m2) = Module (m1 ++ m2)
}
