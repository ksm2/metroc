module MetroLang.Lang.Error (parseError) where

import MetroLang.Lang.Exception
import MetroLang.Lang.Keywords
import MetroLang.Lang.Token

parseError :: Token -> P a
parseError token =
  getLineNo `thenP` \line ->
    getColNo `thenP` \col ->
      getInputFile `thenP` \input ->
        getInputContent `thenP` \content ->
          let start = startOfToken token col
           in failP $ "Parse error in " ++ input ++ " on line " ++ show line ++ ", column " ++ show start ++ ": Unexpected " ++ describeToken token ++ ":\n\n" ++ underlineString content line start col

underlineString :: String -> Int -> Int -> Int -> String
underlineString text line start end =
  let textLines = lines text
      contextLines = 3
      firstLine = max 1 (line - contextLines)
      lastLine = min (length textLines) (line + contextLines)
      maxLength = length (show lastLine)
      visibleLines = ((take (lastLine - firstLine + 1)) . (drop (firstLine - 1))) textLines
      indexedLines = zipWith (\idx line -> " " ++ padLeft maxLength idx ++ " | " ++ line) [firstLine .. lastLine] visibleLines
      scatteredLines = insertAt (line - firstLine + 1) (" " ++ repeatSpaces maxLength ++ " | " ++ underline start end) indexedLines
   in unlines scatteredLines

underline :: Int -> Int -> String
underline start end =
  repeatSpaces (start - 1) ++ take (end - start) strokes
  where
    strokes = repeat '~'

describeToken :: Token -> String
describeToken TokenEOF = "end of file"
describeToken TokenEOS = "end of statement"
describeToken t = drop 5 $ show t

startOfToken :: Token -> Int -> Int
startOfToken (TokenIdentifier iden) col = col - (length iden)
startOfToken token col
  | token == TokenEOF || token == TokenEOS = col
  | isKeywordToken token = col - (length (show token)) + 5
  | otherwise = col - 1

padLeft :: Int -> Int -> String
padLeft maxLength num =
  let str = show num
      actualLength = length str
   in case actualLength < maxLength of
        True -> repeatSpaces (maxLength - actualLength) ++ str
        False -> str

repeatSpaces :: Int -> String
repeatSpaces n = replicate n ' '

insertAt :: Int -> a -> [a] -> [a]
insertAt n newElement xs =
  let (prefix, suffix) = splitAt n xs
   in prefix ++ [newElement] ++ suffix
