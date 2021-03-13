module MetroLang.Lang.Highlight (highlight) where

import Data.Char
import MetroLang.Lang.Keywords

type Highlighter = String -> String

highlight :: Highlighter
highlight [] = []
highlight s@('/' : '/' : _) = highlightPart inLightGray (\c -> c /= '\n') s
highlight s@('/' : '*' : _) = highlightMultiLineComment "" s
highlight ('"' : cs) = highlightString "\"" cs
highlight s@(c : cs)
  | isAlpha c = highlightIdentifier s
  | isDigit c = highlightNumber s
  | isComma c = highlightPart inOrange isComma s
  | otherwise = c : highlight cs

highlightMultiLineComment :: String -> String -> String
highlightMultiLineComment str ('*' : '/' : cs) = inItalicLightGreen (str ++ "*/") ++ highlight cs
highlightMultiLineComment str ('\n' : cs) = inItalicLightGreen str ++ "\n" ++ highlightMultiLineComment "" cs
highlightMultiLineComment str (c : cs) = highlightMultiLineComment (str ++ [c]) cs
highlightMultiLineComment str [] = inItalicLightGreen str

highlightString :: String -> String -> String
highlightString str ('\\' : '"' : cs) = highlightString (str ++ "\\\"") cs
highlightString str ('"' : cs) = inLightGreen (str ++ "\"") ++ highlight cs
highlightString str ('\n' : cs) = inLightGreen str ++ "\n" ++ highlightString "" cs
highlightString str (c : cs) = highlightString (str ++ [c]) cs
highlightString str [] = inLightGreen str

highlightPart :: Highlighter -> (Char -> Bool) -> String -> String
highlightPart highlighter cond str =
  highlighter match ++ highlight rest
  where
    (match, rest) = span cond str

highlightIdentifier :: Highlighter
highlightIdentifier str =
  highlightPart highlighter isAlphaNum str
  where
    highlighter iden = if isKeywordString iden then inBoldOrange iden else iden

highlightNumber :: Highlighter
highlightNumber ('0' : 'x' : cs) =
  inBlue ("0x" ++ match) ++ highlightNumberSuffix rest
  where
    (match, rest) = span (\c -> isDigit c || elem c "abcdefABCDEF") cs
highlightNumber ('0' : 'b' : cs) =
  inBlue ("0b" ++ match) ++ highlightNumberSuffix rest
  where
    (match, rest) = span (\c -> elem c "01") cs
highlightNumber cs =
  inBlue match ++ highlightNumberSuffix rest
  where
    (match, rest) = span (\c -> isDigit c || elem c "._") cs

highlightNumberSuffix :: Highlighter
highlightNumberSuffix ('X' : 'S' : cs) = inBlue "XS" ++ highlight cs
highlightNumberSuffix s@(c : cs)
  | elem c "USBWL" = inBlue [c] ++ highlight cs
  | otherwise = highlight s
highlightNumberSuffix [] = []

isComma :: Char -> Bool
isComma ',' = True
isComma _ = False

inBlue :: Highlighter
inBlue text = "\x1b[38;2;104;151;187m" ++ text ++ "\x1b[m"

inBoldOrange :: Highlighter
inBoldOrange text = "\x1b[1;38;2;204;120;50m" ++ text ++ "\x1b[m"

inOrange :: Highlighter
inOrange text = "\x1b[38;2;204;120;50m" ++ text ++ "\x1b[m"

inLightGray :: Highlighter
inLightGray text = "\x1b[3;38;2;128;128;128m" ++ text ++ "\x1b[m"

inLightGreen :: Highlighter
inLightGreen text = "\x1b[38;2;106;135;89m" ++ text ++ "\x1b[m"

inItalicLightGreen :: Highlighter
inItalicLightGreen text = "\x1b[3;38;2;98;151;85m" ++ text ++ "\x1b[m"
