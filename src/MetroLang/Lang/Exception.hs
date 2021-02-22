module MetroLang.Lang.Exception where

data ParseResult a = Ok a | Failed String

type LineNumber = Int

type P a = String -> LineNumber -> ParseResult a

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l ->
  case m s l of
    Ok a -> k a s l
    Failed e -> Failed e

returnP :: a -> P a
returnP a = \s l -> Ok a

failP :: String -> P a
failP err = \s l -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s l ->
  case m s l of
    Ok a -> Ok a
    Failed e -> k e s l

getLineNo :: P LineNumber
getLineNo = \s l -> Ok l
