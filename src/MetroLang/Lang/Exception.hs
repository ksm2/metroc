module MetroLang.Lang.Exception where

data ParseResult a = Ok a | Failed String

type ColNumber = Int

type LineNumber = Int

type InputFile = String

type InputContent = String

type P a = String -> ColNumber -> LineNumber -> InputFile -> InputContent -> ParseResult a

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s c l i content ->
  case m s c l i content of
    Ok a -> k a s c l i content
    Failed e -> Failed e

returnP :: a -> P a
returnP a s c l i content = Ok a

failP :: String -> P a
failP err s c l i content = Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k s c l i content =
  case m s c l i content of
    Ok a -> Ok a
    Failed e -> k e s c l i content

getColNo :: P ColNumber
getColNo s c l i content = Ok c

getLineNo :: P LineNumber
getLineNo s c l i content = Ok l

getInputFile :: P InputFile
getInputFile s c l i content = Ok i

getInputContent :: P InputContent
getInputContent s c l i = Ok
