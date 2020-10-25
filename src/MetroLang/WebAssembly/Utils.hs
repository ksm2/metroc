module MetroLang.WebAssembly.Utils (merge) where

import MetroLang.WebAssembly.AST

merge :: Module -> Module -> Module
merge (Mod d1) (Mod d2) = Mod (d1 ++ d2)
