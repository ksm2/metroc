module MetroLang.Lang.Exception where

data ParseResult a = Ok a | Failed String

type ColNumber = Int

type LineNumber = Int

type P a = String -> ColNumber -> LineNumber -> ParseResult a

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s c l ->
  case m s c l of
    Ok a -> k a s c l
    Failed e -> Failed e

returnP :: a -> P a
returnP a = \s c l -> Ok a

failP :: String -> P a
failP err = \s c l -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s c l ->
  case m s c l of
    Ok a -> Ok a
    Failed e -> k e s c l

getColNo :: P ColNumber
getColNo = \s c l -> Ok c

getLineNo :: P LineNumber
getLineNo = \s c l -> Ok l
