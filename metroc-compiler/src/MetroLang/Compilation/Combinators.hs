module MetroLang.Compilation.Combinators where

import MetroLang.Compilation.Context

many :: (b -> Compiler a) -> [b] -> Compiler [a]
many _ [] = return []
many singleCompiler (x : xs) =
  do
    compiled <- singleCompiler x
    rest <- many singleCompiler xs
    return $ compiled : rest

flatMany :: (b -> Compiler [a]) -> [b] -> Compiler [a]
flatMany _ [] = return []
flatMany singleCompiler (x : xs) =
  do
    compiled <- singleCompiler x
    rest <- flatMany singleCompiler xs
    return $ compiled ++ rest
