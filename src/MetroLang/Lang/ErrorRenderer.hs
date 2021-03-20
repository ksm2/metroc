module MetroLang.Lang.ErrorRenderer (renderError) where

import Data.Char
import Data.Map ((!))
import qualified Data.Map as M
import MetroLang.Lang.Error
import MetroLang.Lang.Highlight
import MetroLang.Lang.Lexeme
import MetroLang.Lang.Token
import MetroLang.Location

renderError :: M.Map Source String -> MetroError -> String
renderError sources = \case
  LexerError loc@(SourceLocation source start end) ch -> theRenderError ("Unexpected char \"" ++ ch : "\"") (sources ! source) loc
  ParserError (L loc@(SourceLocation source start end) t _) -> theRenderError ("Unexpected " ++ describeToken t) (sources ! source) loc

theRenderError :: String -> String -> SourceLocation -> String
theRenderError reason content (SourceLocation (Source source) (Position line column) (Position _ end)) =
  "Error in " ++ source ++ " on line " ++ show line ++ ", column " ++ show column ++ ": " ++ reason ++ ":\n\n" ++ underlineString content line column end

underlineString :: String -> Int -> Int -> Int -> String
underlineString text line start end =
  let textLines = lines $ highlight text
      contextLines = 5
      firstLine = max 1 (line - contextLines)
      lastLine = min (length textLines) (line + contextLines)
      maxLength = length (show lastLine)
      visibleLines = (take (lastLine - firstLine + 1) . drop (firstLine - 1)) textLines
      indexedLines = zipWith (\idx l -> " " ++ inGray (padLeft maxLength idx ++ " | ") ++ l) [firstLine .. lastLine] visibleLines
      scatteredLines = insertAt (line - firstLine + 1) (" " ++ repeatSpaces maxLength ++ inGray " | " ++ underline start end) indexedLines
   in unlines scatteredLines

underline :: Int -> Int -> String
underline start end =
  repeatSpaces (start - 1) ++ inRed (take (end - start) strokes)
  where
    strokes = repeat '^'

inRed :: String -> String
inRed text = "\x1b[91;1m" ++ text ++ "\x1b[m"

inGray :: String -> String
inGray text = "\x1b[90m" ++ text ++ "\x1b[m"

repeatSpaces :: Int -> String
repeatSpaces n = replicate n ' '

insertAt :: Int -> a -> [a] -> [a]
insertAt n newElement xs =
  let (prefix, suffix) = splitAt n xs
   in prefix ++ [newElement] ++ suffix

padLeft :: Int -> Int -> String
padLeft maxLength num =
  let str = show num
      actualLength = length str
   in if actualLength < maxLength then repeatSpaces (maxLength - actualLength) ++ str else str

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
