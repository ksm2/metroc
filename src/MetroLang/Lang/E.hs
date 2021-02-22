module MetroLang.Lang.E where

data E a = Ok a | Failed String

thenE :: E a -> (a -> E b) -> E b
m `thenE` k =
  case m of
    Ok a -> k a
    Failed e -> Failed e

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE err = Failed err

catchE :: E a -> (String -> E a) -> E a
catchE m k =
  case m of
    Ok a -> Ok a
    Failed e -> k e
