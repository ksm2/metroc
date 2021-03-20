module MetroLang.Lang.Error (parseError) where

import Data.Char
import MetroLang.Lang.Lexer
import MetroLang.Lang.Token

parseError :: Lexeme -> Alex a
parseError = \case
  L pos token str ->
    alexRenderError pos ("Unexpected " ++ describeToken token) str

describeToken :: Token -> String
describeToken TokenEOF = "end of file"
describeToken TokenEOS = "end of statement"
describeToken (TokenInt _) = "integer literal"
describeToken (TokenString _) = "string literal"
describeToken TokenTBool = "Bool"
describeToken TokenTIntXS = "IntXS"
describeToken TokenTByte = "Byte"
describeToken TokenTIntS = "IntS"
describeToken TokenTWord = "Word"
describeToken TokenTInt = "Int"
describeToken TokenTUInt = "UInt"
describeToken TokenTIntL = "IntL"
describeToken TokenTUIntL = "UIntL"
describeToken TokenTFloat = "Float"
describeToken TokenTFloatL = "FloatL"
describeToken TokenTChar = "Char"
describeToken TokenTString = "String"
describeToken t
  | isKeyword t = map toLower (drop 5 $ show t) ++ " keyword"
  | otherwise = drop 5 $ show t
