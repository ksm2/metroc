{
module MetroLang.Lang.Lexer (lexer, runLexer) where

import qualified Data.Bits
import Data.Char (ord)
import Data.Word (Word8)
import MetroLang.Lang.Error
import MetroLang.Lang.Lexeme
import MetroLang.Lang.Parlex
import MetroLang.Lang.Token
import MetroLang.Location
import MetroLang.Utils.Int
}

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
  <0,newline> $white+		{ skip }
  <0> $newline          { begin newline }

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

  <0> "/*"                    { begin multilinecomment }
  <multilinecomment> "*/"     { begin 0 }
  <multilinecomment> .        { skip }
  <multilinecomment> $newline { skip }

  <0> "//"                      { begin singlelinecomment }
  <singlelinecomment> $newline  { begin 0 }
  <singlelinecomment> [^]       { skip }

  <0> \"                { begin string }
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

  <0> @identifier       { mkL (const TokenIdentifier) }
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

  <0> $white+			      { skip }
{
utf8Encode' :: Char -> (Word8, [Word8])
utf8Encode' c = case go (ord c) of
                  (x, xs) -> (fromIntegral x, map fromIntegral xs)
 where
  go oc
   | oc <= 0x7f       = ( oc
                        , [
                        ])

   | oc <= 0x7ff      = ( 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , [0x80 + oc Data.Bits..&. 0x3f
                        ])

   | oc <= 0xffff     = ( 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , [0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ])
   | otherwise        = ( 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , [0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ])

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes (p,c,_ps,s) = (p,c,[],s)

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p,c,(b:bs),s) = Just (b,(p,c,bs,s))
alexGetByte (_,_,[],[]) = Nothing
alexGetByte (p,_,[],(c:s))  = let p' = alexMove p c
                              in case utf8Encode' c of
                                   (b, bs) -> p' `seq`  Just (b, (p', c, bs, s))

alexMove :: Position -> Char -> Position
alexMove (Position l c) '\t' = Position  l     (c+alex_tab_size-((c-1) `mod` alex_tab_size))
alexMove (Position l _) '\n' = Position (l+1)   1
alexMove (Position l c) _    = Position  l     (c+1)

type AlexInput = (Position,     -- current position,
                  Char,         -- previous char
                  [Byte],       -- rest of the bytes for the current char
                  String)       -- current input string

metroGetInput :: Parlex AlexInput
metroGetInput
 = Parlex $ \s@ParlexState{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp__} ->
        Right (s, (pos,c,bs,inp__))

metroSetInput :: AlexInput -> Parlex ()
metroSetInput (pos,c,bs,inp__)
 = Parlex $ \s -> case s{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp__} of
                  state__@(ParlexState{}) -> Right (state__, ())

metroGetStartCode :: Parlex Int
metroGetStartCode = Parlex $ \s@ParlexState{alex_scd=sc} -> Right (s, sc)

metroSetStartCode :: Int -> Parlex ()
metroSetStartCode sc = Parlex $ \s -> Right (s{alex_scd=sc}, ())

metroGetSource :: Parlex Source
metroGetSource = Parlex $ \s@ParlexState{alex_source=ust} -> Right (s,ust)

metroSetSource :: Source -> Parlex ()
metroSetSource source = Parlex $ \s -> Right (s{alex_source=source}, ())

metroGetString :: Parlex String
metroGetString = Parlex $ \s@ParlexState{alex_string=ust} -> Right (s,ust)

metroSetString :: String -> Parlex ()
metroSetString str = Parlex $ \s -> Right (s{alex_string=str}, ())

mkL :: (String -> Token) -> MetroAction Lexeme
mkL t (p,_,_,rest) len =
  do
    s <- metroGetSource
    return (L (mkSourceLocation p s len) (t str) str)
    where str = (take len rest)

stringchar :: MetroAction Lexeme
stringchar p@(_,_,_,rest) len =
  addchar (take len rest) p len

addchar :: String -> MetroAction Lexeme
addchar n _ _ =
  do
    s <- metroGetString
    metroSetString $ s ++ n
    monadScan

endstring :: MetroAction Lexeme
endstring (p,_,_,_) _ =
  do
    src <- metroGetSource
    str <- metroGetString
    res <- return (L (mkSourceLocation p src (length str)) (TokenString str) str)
    metroSetString ""
    return res

monadScan :: Parlex Lexeme
monadScan = do
  inp__ <- metroGetInput
  sc <- metroGetStartCode
  case alexScan inp__ sc of
    AlexEOF -> return (L undefined TokenEOF "")
    AlexError (p,_,_,s) -> do
        source <- metroGetSource
        Parlex $ const $ Left $ LexerError (mkSourceLocation p source 1) (head s)
    AlexSkip  inp__' _len -> do
        metroSetInput inp__'
        monadScan
    AlexToken inp__' len action -> do
        metroSetInput inp__'
        action (ignorePendingBytes inp__) len

skip :: MetroAction Lexeme
skip _input _len = monadScan

begin :: Int -> MetroAction Lexeme
begin code _input _len = do
  metroSetStartCode code
  monadScan

type MetroAction result = AlexInput -> Int -> Parlex result

andBegin :: MetroAction result -> Int -> MetroAction result
(action `andBegin` code) input__ len = do
  metroSetStartCode code
  action input__ len

ignoreLast :: String -> String
ignoreLast [] = []
ignoreLast [_] = []
ignoreLast (c:cs) = c : ignoreLast cs

lexer :: (Lexeme -> Parlex a) -> Parlex a
lexer = (monadScan >>=)

runLexer :: Source -> String -> Parlex a -> Either MetroError a
runLexer source contents (Parlex f)
   = case f (ParlexState {alex_bytes = [],
                          alex_pos = startPos,
                          alex_inp = contents,
                          alex_chr = '\n',
                          alex_source = source,
                          alex_string = "",
                          alex_scd = 0}) of Left err -> Left err
                                            Right ( _, a ) -> Right a
}
