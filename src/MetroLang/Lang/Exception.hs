module MetroLang.Lang.Exception where

data ParseResult a = Ok a | Failed String

type ColNumber = Int

type LineNumber = Int

type InputFile = String

type P a = String -> ColNumber -> LineNumber -> InputFile -> ParseResult a

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s c l i ->
  case m s c l i of
    Ok a -> k a s c l i
    Failed e -> Failed e

returnP :: a -> P a
returnP a = \s c l i -> Ok a

failP :: String -> P a
failP err = \s c l i -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s c l i ->
  case m s c l i of
    Ok a -> Ok a
    Failed e -> k e s c l i

getColNo :: P ColNumber
getColNo = \s c l i -> Ok c

getLineNo :: P LineNumber
getLineNo = \s c l i -> Ok l

getInputFile :: P InputFile
getInputFile = \s c l i -> Ok i
