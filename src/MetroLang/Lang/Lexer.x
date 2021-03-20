{
module MetroLang.Lang.Lexer (Alex, AlexPosn(..), Lexeme(..), alexRenderError, getInputContent, getInputFile, lexer, runLexer) where

import MetroLang.Lang.ErrorRenderer
import MetroLang.Lang.Token
import MetroLang.Location
}

%wrapper "monadUserState"

$white      = [\ \t\f\v]
$newline    = [\r\n]

$large      = [A-Z \xc0-\xd6 \xd8-\xde]
$small      = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha      = [$small $large]

$digit      = 0-9
$nonzerodigit = 1-9
$bindig     = [01]
$octdig     = [0-7]
$hexdig     = [0-9A-Fa-f]

$idchar     = [$alpha $digit]
@identifier = $alpha $idchar*
@decint     = $nonzerodigit $digit* | 0
@decuint    = @decint U
@decbyte    = @decint B
@hexint     = 0x $hexdig+
@hexuint    = @hexint U
@hexbyte    = @hexint B
@octint     = 0o $octdig+
@octuint    = @octint U
@octbyte    = @octint B
@binint     = 0b $bindig+
@binuint    = @binint U
@binbyte    = @binint B

tokens :-
  <0,newline> $white+		{ metroSkip }
  <0> $newline          { metroBegin newline }

  <0> and               { mkL $ \s -> TokenAnd }
  <0> as                { mkL $ \s -> TokenAs }
  <0> assert            { mkL $ \s -> TokenAssert }
  <0> class             { mkL $ \s -> TokenClass }
  <0> const             { mkL $ \s -> TokenConst }
  <0> else              { mkL $ \s -> TokenElse }
  <0> enum              { mkL $ \s -> TokenEnum }
  <0> export            { mkL $ \s -> TokenExport }
  <0> extends           { mkL $ \s -> TokenExtends }
  <0> external          { mkL $ \s -> TokenExternal }
  <0> false             { mkL $ \s -> TokenFalse }
  <0> fn                { mkL $ \s -> TokenFn }
  <0> for               { mkL $ \s -> TokenFor }
  <0> hide              { mkL $ \s -> TokenHide }
  <0> if                { mkL $ \s -> TokenIf }
  <0> it                { mkL $ \s -> TokenIt }
  <0> impl              { mkL $ \s -> TokenImpl }
  <0> import            { mkL $ \s -> TokenImport }
  <0> interface         { mkL $ \s -> TokenInterface }
  <0> is                { mkL $ \s -> TokenIs }
  <0> let               { mkL $ \s -> TokenLet }
  <0> match             { mkL $ \s -> TokenMatch }
  <0> not               { mkL $ \s -> TokenNot }
  <0> null              { mkL $ \s -> TokenNull }
  <0> or                { mkL $ \s -> TokenOr }
  <0> return            { mkL $ \s -> TokenReturn }
  <0> static            { mkL $ \s -> TokenStatic }
  <0> test              { mkL $ \s -> TokenTest }
  <0> this              { mkL $ \s -> TokenThis }
  <0> true              { mkL $ \s -> TokenTrue }
  <0> unsafe            { mkL $ \s -> TokenUnsafe }
  <0> while             { mkL $ \s -> TokenWhile }

  <0> Bool              { mkL $ \s -> TokenTBool }
  <0> IntXS             { mkL $ \s -> TokenTIntXS }
  <0> Byte              { mkL $ \s -> TokenTByte }
  <0> IntS              { mkL $ \s -> TokenTIntS }
  <0> Word              { mkL $ \s -> TokenTWord }
  <0> Int               { mkL $ \s -> TokenTInt }
  <0> UInt              { mkL $ \s -> TokenTUInt }
  <0> IntL              { mkL $ \s -> TokenTIntL }
  <0> UIntL             { mkL $ \s -> TokenTUIntL }
  <0> Float             { mkL $ \s -> TokenTFloat }
  <0> FloatL            { mkL $ \s -> TokenTFloatL }
  <0> Char              { mkL $ \s -> TokenTChar }
  <0> String            { mkL $ \s -> TokenTString }

  <0> "/*"                    { metroBegin multilinecomment }
  <multilinecomment> "*/"     { metroBegin 0 }
  <multilinecomment> .        { metroSkip }
  <multilinecomment> $newline { metroSkip }

  <0> "//"                      { metroBegin singlelinecomment }
  <singlelinecomment> $newline  { metroBegin 0 }
  <singlelinecomment> [^]       { metroSkip }

  <0> \"                { metroBegin string }
  <string> "\t"         { addchar "\t" }
  <string> "\n"         { addchar "\n" }
  <string> "\f"         { addchar "\f" }
  <string> "\v"         { addchar "\v" }
  <string> "\r"         { addchar "\r" }
  <string> "\e"         { addchar "\x1b" }
  <string> [^"]         { stringchar }
  <string> \"           { endstring `andBegin` 0 }

  <0> "{"               { mkL $ \s -> TokenLBrace }
  <0> "}"               { mkL $ \s -> TokenRBrace }
  <0> "("               { mkL $ \s -> TokenLParen }
  <0> ")"               { mkL $ \s -> TokenRParen }
  <0> ","               { mkL $ \s -> TokenComma }

  <0,newline> "!="      { mkL (const TokenExclEq) `andBegin` 0 }
  <0,newline> "%="      { mkL (const TokenRemEq) `andBegin` 0 }
  <0,newline> "%>="     { mkL (const TokenRemGtEq) `andBegin` 0 }
  <0,newline> "%>"      { mkL (const TokenRemGt) `andBegin` 0 }
  <0,newline> "%"       { mkL (const TokenRem) `andBegin` 0 }
  <0,newline> "&="      { mkL (const TokenAmpEq) `andBegin` 0 }
  <0,newline> "&"       { mkL (const TokenAmp) `andBegin` 0 }
  <0,newline> "*="      { mkL (const TokenMulEq) `andBegin` 0 }
  <0,newline> "*"       { mkL (const TokenMul) `andBegin` 0 }
  <0,newline> "+="      { mkL (const TokenPlusEq) `andBegin` 0 }
  <0,newline> "+"       { mkL (const TokenPlus) `andBegin` 0 }
  <0,newline> ","       { mkL (const TokenComma) `andBegin` 0 }
  <0,newline> "-="      { mkL (const TokenMinusEq) `andBegin` 0 }
  <0,newline> "-"       { mkL (const TokenMinus) `andBegin` 0 }
  <0,newline> "."       { mkL (const TokenDot) `andBegin` 0 }
  <0,newline> "/="      { mkL (const TokenDivEq) `andBegin` 0 }
  <0> "/"               { mkL (const TokenDiv) `andBegin` 0 }
  <0,newline> ":="      { mkL (const TokenColonEq) `andBegin` 0 }
  <0,newline> ":"       { mkL (const TokenColon) `andBegin` 0 }
  <0,newline> "<%="     { mkL (const TokenLtRemEq) `andBegin` 0 }
  <0,newline> "<%"      { mkL (const TokenLtRem) `andBegin` 0 }
  <0,newline> "<<="     { mkL (const TokenLtLtEq) `andBegin` 0 }
  <0,newline> "<<"      { mkL (const TokenLtLt) `andBegin` 0 }
  <0,newline> "<="      { mkL (const TokenLtEq) `andBegin` 0 }
  <0,newline> "<"       { mkL (const TokenLt) `andBegin` 0 }
  <0,newline> "=="      { mkL (const TokenEqEq) `andBegin` 0 }
  <0,newline> "=>"      { mkL (const TokenEqGt) `andBegin` 0 }
  <0,newline> "="       { mkL (const TokenEq) `andBegin` 0 }
  <0,newline> ">="      { mkL (const TokenGtEq) `andBegin` 0 }
  <0,newline> ">>="     { mkL (const TokenGtGtEq) `andBegin` 0 }
  <0,newline> ">>"      { mkL (const TokenGtGt) `andBegin` 0 }
  <0,newline> ">"       { mkL (const TokenGt) `andBegin` 0 }
  <0,newline> "?."      { mkL (const TokenQDot) `andBegin` 0 }
  <0,newline> "?"       { mkL (const TokenQ) `andBegin` 0 }
  <0,newline> "["       { mkL (const TokenLBrack) `andBegin` 0 }
  <0,newline> "]"       { mkL (const TokenRBrack) `andBegin` 0 }
  <0,newline> "^="      { mkL (const TokenCaretEq) `andBegin` 0 }
  <0,newline> "^"       { mkL (const TokenCaret) `andBegin` 0 }
  <0,newline> "|="      { mkL (const TokenBarEq) `andBegin` 0 }
  <0,newline> "|"       { mkL (const TokenBar) `andBegin` 0 }
  <0,newline> "~"       { mkL (const TokenTilde) `andBegin` 0 }
  <newline>  ()         { mkL (const TokenEOS) `andBegin` 0 }

  <0> @identifier       { mkL TokenIdentifier }
  <0> @decuint          { mkL $ TokenUInt . read . ignoreLast }
  <0> @decbyte          { mkL $ TokenByte . read . ignoreLast }
  <0> @decint           { mkL $ TokenInt . read }
  <0> @hexuint          { mkL $ TokenUInt . parseHex . ignoreLast . (drop 2) }
  <0> @hexbyte          { mkL $ TokenByte . parseHex . ignoreLast . (drop 2) }
  <0> @hexint           { mkL $ TokenInt . parseHex . (drop 2) }
  <0> @octuint          { mkL $ TokenUInt . parseOct . ignoreLast . (drop 2) }
  <0> @octbyte          { mkL $ TokenByte . parseOct . ignoreLast . (drop 2) }
  <0> @octint           { mkL $ TokenInt . parseOct . (drop 2) }
  <0> @binuint          { mkL $ TokenUInt . parseBin . ignoreLast . (drop 2) }
  <0> @binbyte          { mkL $ TokenByte . parseBin . ignoreLast . (drop 2) }
  <0> @binint           { mkL $ TokenInt . parseBin . (drop 2) }

<0> $white+			{ metroSkip }
{
data AlexUserState =
  AlexUserState { inputFile    :: String
                , inputContent :: String
                , stringStack  :: String
                }

getInputFile :: Alex String
getInputFile = inputFile <$> alexGetUserState

getInputContent :: Alex String
getInputContent = inputContent <$> alexGetUserState

alexEOF :: Alex Lexeme
alexEOF = return (L undefined TokenEOF "")
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "" "" ""

data Lexeme = L SourceLocation Token String

mkL :: (String -> Token) -> AlexInput -> Int -> Alex Lexeme
mkL t (p,_,_,rest) len =
  do
    s <- alexGetUserState
    return (L (convertPosn p s len) (t str) str)
    where str = (take len rest)

convertPosn :: AlexPosn -> AlexUserState -> Int -> SourceLocation
convertPosn (AlexPn _ line col) (AlexUserState source content _) len = SourceLocation source content (Position line col) (Position line (col + len))

stringchar :: AlexInput -> Int -> Alex Lexeme
stringchar p@(_,_,_,rest) len =
  addchar (take len rest) p len

addchar :: String -> AlexInput -> Int -> Alex Lexeme
addchar n _ _ =
  do
    s <- alexGetUserState
    alexSetUserState s{stringStack = stringStack s ++ n}
    metroMonadScan

endstring :: AlexInput -> Int -> Alex Lexeme
endstring (p,_,_,_) _ =
  do
    s <- alexGetUserState
    let str = stringStack s
    res <- return (L (convertPosn p s (length str)) (TokenString str) str)
    alexSetUserState s{stringStack = ""}
    return res

alexRenderError :: SourceLocation -> String -> String -> Alex a
alexRenderError loc msg str = alexError $ renderError msg loc

metroMonadScan :: Alex Lexeme
metroMonadScan = do
  inp__ <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp__ sc of
    AlexEOF -> alexEOF
    AlexError (p,_,_,s) -> do
        source <- alexGetUserState
        alexRenderError (convertPosn p source 0) ("Lexical error, unexpected " ++ head s : "") [head s]
    AlexSkip  inp__' _len -> do
        alexSetInput inp__'
        metroMonadScan
    AlexToken inp__' len action -> do
        alexSetInput inp__'
        action (ignorePendingBytes inp__) len

metroSkip _input _len = metroMonadScan

metroBegin code _input _len = do alexSetStartCode code; metroMonadScan

parseHex = parseInt 16
parseOct = parseInt 8
parseBin = parseInt 2

parseInt :: Int -> String -> Int
parseInt _  "" = 0
parseInt _  "0" = 0
parseInt _  [c] = parseIntChar c
parseInt base cs = parseIntChar (last cs) + base * parseInt base (init cs)

parseIntChar :: Char -> Int
parseIntChar '0' = 0
parseIntChar '1' = 1
parseIntChar '2' = 2
parseIntChar '3' = 3
parseIntChar '4' = 4
parseIntChar '5' = 5
parseIntChar '6' = 6
parseIntChar '7' = 7
parseIntChar '8' = 8
parseIntChar '9' = 9
parseIntChar 'a' = 10
parseIntChar 'A' = 10
parseIntChar 'b' = 11
parseIntChar 'B' = 11
parseIntChar 'c' = 12
parseIntChar 'C' = 12
parseIntChar 'd' = 13
parseIntChar 'D' = 13
parseIntChar 'e' = 14
parseIntChar 'E' = 14
parseIntChar 'f' = 15
parseIntChar 'F' = 15
parseIntChar x = error $ "Illegal int char: " ++ [x]

ignoreLast :: String -> String
ignoreLast [] = []
ignoreLast [_] = []
ignoreLast (c:cs) = c : ignoreLast cs

lexer :: (Lexeme -> Alex a) -> Alex a
lexer = (metroMonadScan >>=)

runLexer :: String -> String -> Alex a -> Either String a
runLexer fileInput contents calc = runAlex contents $
  alexSetUserState (AlexUserState fileInput contents "") >> calc
}
