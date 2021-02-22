module MetroLang.Lang.Exception where

data ParseResult a = Ok a | Failed String

type P a = String -> ParseResult a

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s ->
  case m s of
    Ok a -> k a s
    Failed e -> Failed e

returnP :: a -> P a
returnP a = \s -> Ok a

failP :: String -> P a
failP err = \s -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s ->
  case m s of
    Ok a -> Ok a
    Failed e -> k e s
