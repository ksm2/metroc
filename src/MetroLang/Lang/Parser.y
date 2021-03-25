{
module MetroLang.Lang.Parser (parse, merge) where

import MetroLang.Lang.Error
import MetroLang.Lang.Lexeme
import MetroLang.Lang.Lexer
import MetroLang.Lang.Model
import MetroLang.Lang.Parlex
import MetroLang.Lang.Pretty
import MetroLang.Lang.Token
import MetroLang.Location
}

%name calc
%tokentype { Lexeme }
%monad { Parlex }
%lexer { lexer } { L _ TokenEOF _ }
%error { parseError }

%token
      and               { L _ TokenAnd _ }
      as                { L _ TokenAs _ }
      assert            { L _ TokenAssert _ }
      class             { L _ TokenClass _ }
      const             { L _ TokenConst _ }
      else              { L _ TokenElse _ }
      enum              { L _ TokenEnum _ }
      export            { L _ TokenExport _ }
      extends           { L _ TokenExtends _ }
      external          { L _ TokenExternal _ }
      false             { L _ TokenFalse _ }
      fn                { L _ TokenFn _ }
      for               { L _ TokenFor _ }
      hide              { L _ TokenHide _ }
      if                { L _ TokenIf _ }
      it                { L _ TokenIt _ }
      impl              { L _ TokenImpl _ }
      import            { L _ TokenImport _ }
      interface         { L _ TokenInterface _ }
      is                { L _ TokenIs _ }
      let               { L _ TokenLet _ }
      match             { L _ TokenMatch _ }
      not               { L _ TokenNot _ }
      null              { L _ TokenNull _ }
      or                { L _ TokenOr _ }
      return            { L _ TokenReturn _ }
      static            { L _ TokenStatic _ }
      test              { L _ TokenTest _ }
      this              { L _ TokenThis _ }
      true              { L _ TokenTrue _ }
      unsafe            { L _ TokenUnsafe _ }
      while             { L _ TokenWhile _ }

      Bool      { L _ TokenTBool _ }
      IntXS     { L _ TokenTIntXS _ }
      Byte      { L _ TokenTByte _ }
      IntS      { L _ TokenTIntS _ }
      Word      { L _ TokenTWord _ }
      Int       { L _ TokenTInt _ }
      UInt      { L _ TokenTUInt _ }
      IntL      { L _ TokenTIntL _ }
      UIntL     { L _ TokenTUIntL _ }
      Float     { L _ TokenTFloat _ }
      FloatL    { L _ TokenTFloatL _ }
      Char      { L _ TokenTChar _ }
      String    { L _ TokenTString _ }

      '!='      { L _ TokenExclEq _ }
      '%'       { L _ TokenRem _ }
      '%='      { L _ TokenRemEq _ }
      '%>'      { L _ TokenRemGt _ }
      '%>='     { L _ TokenRemGtEq _ }
      '&'       { L _ TokenAmp _ }
      '&='      { L _ TokenAmpEq _ }
      '('       { L _ TokenLParen _ }
      ')'       { L _ TokenRParen _ }
      '*'       { L _ TokenMul _ }
      '*='      { L _ TokenMulEq _ }
      '+'       { L _ TokenPlus _ }
      '+='      { L _ TokenPlusEq _ }
      ','       { L _ TokenComma _ }
      '-'       { L _ TokenMinus _ }
      '-='      { L _ TokenMinusEq _ }
      '.'       { L _ TokenDot _ }
      '/'       { L _ TokenDiv _ }
      '/='      { L _ TokenDivEq _ }
      ':'       { L _ TokenColon _ }
      ':='      { L _ TokenColonEq _ }
      '<%'      { L _ TokenLtRem _ }
      '<%='     { L _ TokenLtRemEq _ }
      '<'       { L _ TokenLt _ }
      '<<'      { L _ TokenLtLt _ }
      '<<='     { L _ TokenLtLtEq _ }
      '<='      { L _ TokenLtEq _ }
      '='       { L _ TokenEq _ }
      '=='      { L _ TokenEqEq _ }
      '=>'      { L _ TokenEqGt _ }
      '>'       { L _ TokenGt _ }
      '>='      { L _ TokenGtEq _ }
      '>>'      { L _ TokenGtGt _ }
      '>>='     { L _ TokenGtGtEq _ }
      '?'       { L _ TokenQ _ }
      '?.'      { L _ TokenQDot _ }
      '['       { L _ TokenLBrack _ }
      ']'       { L _ TokenRBrack _ }
      '^'       { L _ TokenCaret _ }
      '^='      { L _ TokenCaretEq _ }
      '_'       { L _ TokenUnderscore _ }
      '{'       { L _ TokenLBrace _ }
      '|'       { L _ TokenBar _ }
      '|='      { L _ TokenBarEq _ }
      '}'       { L _ TokenRBrace _ }
      '~'       { L _ TokenTilde _ }

      id        { L _ TokenIdentifier _ }
      int       { L _ (TokenInt _) _ }
      uint      { L _ (TokenUInt _) _ }
      byte      { L _ (TokenByte _) _ }
      string    { L _ (TokenString _) _ }

      eos       { L _ TokenEOS _ }

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

TestDeclaration         : test TypeName TestBlock                       { TestDeclaration $2 $3 }
TestBlock               : BodyOpen TestStatements BodyClose             { reverse $2 }
TestStatements          :: { [TestStatement] }
TestStatements          : {- empty -}                                   { [] }
                        | TestStatement                                 { [$1] }
                        | TestStatements TestStatement                  { $2 : $1 }
TestStatement           :: { TestStatement }
TestStatement           : it string Block                               { TestStatement (lexemeStringLiteral $2) $3 }

HideDeclaration         : hide HideableDeclaration                      { HideDeclaration $2 }

ConstDeclaration        : const id '=' Expression EOS                   { ConstDeclaration (lexemeText $2) $4 }

ExternalDeclaration     : external string External EOS                  { ExternalDeclaration (lexemeStringLiteral $2) $3 }
External                :: { External }
External                : fn id Params ReturnType                       { FnExternal (lexemeText $2) $3 $4 }

EnumDeclaration         : enum id TypeArguments EnumBody                { EnumDeclaration (lexemeText $2) $3 $4 }
EnumBody                : BodyOpen EnumItems BodyClose                  { reverse $2 }
EnumItems               : EnumItem                                      { [$1] }
                        | EnumItems EOS EnumItem                        { $3 : $1 }

EnumItem                :: { EnumItem }
EnumItem                : id OptParams                                  { EnumItem (lexemeText $1) $2 }

InterfaceDeclaration    : interface id TypeArguments InterfaceBody      { InterfaceDeclaration (lexemeText $2) $3 $4 }
InterfaceBody           : BodyOpen InterfaceMethods BodyClose           { reverse $2 }
InterfaceMethods        : {- empty -}                                   { [] }
                        | InterfaceMethod                               { [$1] }
                        | InterfaceMethods InterfaceMethod              { $2 : $1 }
InterfaceMethod         :: { InterfaceMethod }
InterfaceMethod         : id Arguments ReturnType EOS                   { InterfaceMethod (lexemeText $1) $2 $3 }

ImplDeclaration         : impl Type for Type ClassBody                  { ImplDeclaration $2 $4 $5 }

ClassDeclaration        : class TypeName TypeArguments OptParams ClassExtension ClassImplementation ClassBody { ClassDeclaration $2 $3 $4 $5 $6 $7 }
ClassExtension          : {- empty -}                                   { [] }
                        | extends TypeList                              { reverse $2 }
ClassImplementation     : {- empty -}                                   { [] }
                        | impl TypeList                                 { reverse $2 }
ClassBody               :: { ClassBody }
ClassBody               : BodyOpen ClassElements BodyClose              { reverse $2 }
ClassElements           : {- empty -}                                   { [] }
                        | ClassElement                                  { [$1] }
                        | ClassElements ClassElement                    { $2 : $1 }
ClassElement            : static id ':=' Expression EOS                 { StaticField (lexemeText $2) $4 }
                        | static MethodSignature Block                  { StaticMethod $2 $3 }
                        | id ':=' Expression EOS                        { Field (lexemeText $1) $3 }
                        | MethodSignature Block                         { Method $1 $2 }

MethodSignature         :: { MethodSignature }
MethodSignature         : id Params ReturnType                          { MethodSignature Safe (lexemeText $1) $2 $3 }
                        | unsafe id Params ReturnType                   { MethodSignature Unsafe (lexemeText $2) $3 $4 }

FnDeclaration           : Safety fn id Params ReturnType Block          { FnDeclaration (lexemeText $3) $1 $4 $5 $6 }
Block                   :: { Block }
Block                   : BodyOpen Statements BodyClose                 { reverse $2 }

Safety                  :: { Safety }
Safety                  : unsafe                                        { Unsafe }
                        | {- empty -}                                   { Safe }

Statements              :: { [Statement] }
Statements              : {- empty -}                                   { [] }
                        | Statement                                     { [$1] }
                        | Statements Statement                          { $2 : $1 }

Statement               :: { Statement }
Statement               : id ':=' Expression EOS                        { AssignStatement (lexemeText $1) $3 }
                        | IfStatement                                   { IfStatement $1 }
                        | while Expression Block                        { WhileStatement $2 $3 }
                        | AssertStatement                               { $1 }
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

AssertStatement         :: { Statement }
AssertStatement         : assert Expression EOS                         { AssertStatement $2 (pretty $2) }
                        | assert Expression ':' string EOS              { AssertStatement $2 (lexemeStringLiteral $4) }

ReturnCondition         : {- empty -}                                   { Nothing }
                        | if Expression                                 { Just $2 }

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
Param                   : id Type                                       { Param (lexemeText $1) $2 }

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
TypeArgument            : id                                            { TypeArgument (lexemeText $1) }

Type                    :: { Type }
Type                    : id                                            { RefType (lexemeText $1) }
                        | PrimitiveType                                 { PrimitiveType $1 }
                        | '[' Type ']'                                  { ArrayType $2 }
                        | Type '<' TypeArgumentList '>'                 { GenericType $1 (reverse $3) }

TypeName                :: { String }
TypeName                : id                                            { lexemeText $1 }
                        | PrimitiveType                                 { tail $ show $1 }

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
FQN                     : id                                            { [lexemeText $1] }
                        | FQN '.' id                                    { lexemeText $3 : $1 }

Expression              :: { Expression }
Expression              : '(' Expression ')'                            { ParenExpression $2 }
                        | Literal                                       { LiteralExpression $1 }
                        | id                                            { VarExpression (lexemeText $1) (lexemeLoc $1) }
                        | this                                          { ThisExpression (lexemeLoc $1) }
                        | null                                          { NullExpression (lexemeLoc $1) }
                        | Expression as Type                            { CastExpression $1 $3 }
                        | id Arguments                                  { CallExpression (lexemeText $1) $2 }
                        | Expression Index                              { IndexExpression $1 $2 }
                        | Expression '.' id Arguments                   { MethodCallExpression $1 (lexemeText $3) $4 }
                        | Expression '.' id                             { AccessExpression $1 (lexemeText $3) }
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
Arguments               : '(' ')'                                       { Arguments [] }
                        | '(' ArgumentList ')'                          { Arguments (reverse $2) }
ArgumentList            : Expression                                    { [$1] }
                        | ArgumentList ','                              { $1 }
                        | ArgumentList ',' Expression                   { $3 : $1 }

Literal                 :: { Literal }
Literal                 : int                                           { let (TokenInt i) = lexemeToken $1 in IntLiteral i (lexemeLoc $1) }
                        | uint                                          { let (TokenUInt i) = lexemeToken $1 in UIntLiteral i (lexemeLoc $1) }
                        | byte                                          { let (TokenByte i) = lexemeToken $1 in ByteLiteral i (lexemeLoc $1) }
                        | string                                        { StringLiteral (lexemeStringLiteral $1) (lexemeLoc $1) }
                        | true                                          { BoolLiteral True (lexemeLoc $1) }
                        | false                                         { BoolLiteral False (lexemeLoc $1) }

BodyOpen                : OptEOS '{' OptEOS                             {}
BodyClose               : OptEOS '}' OptEOS                             {}

OptEOS                  : {- empty -}                                   {}
                        | EOS                                           {}

EOS                     : eos                                           {}
                        | EOS eos                                       {}

{
lexemeStringLiteral :: Lexeme -> String
lexemeStringLiteral lexeme =
  case lexemeToken lexeme of
    TokenString str -> str

parseError :: Lexeme -> Parlex a
parseError l = Parlex $ const $ Left $ ParserError l

parse :: Source -> String -> Either MetroError Module
parse source content = runLexer source content calc

merge :: Module -> Module -> Module
merge (Module m1) (Module m2) = Module (m1 ++ m2)
}
