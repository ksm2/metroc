module MetroLang.WebAssembly.Utils (injectData, merge) where

import MetroLang.Bytes
import MetroLang.WebAssembly.AST

merge :: Module -> Module -> Module
merge (Mod d1) (Mod d2) = Mod (d1 ++ d2)

injectDeclaration :: Declaration -> Module -> Module
injectDeclaration x (Mod xs) = Mod (x:xs)

injectData :: Int -> String -> Module -> Module
injectData i str m =
  let e = Method "const" I32 [Lit (toInteger i)]
      len = length str
  in  injectDeclaration (Data e (addString str (addInt32 (fromIntegral len) []))) m
