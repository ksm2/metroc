module MetroLang.Lang.Lexeme where

import MetroLang.Lang.Token
import MetroLang.Location

data Lexeme = L {lexemeLoc :: SourceLocation, lexemeToken :: Token, lexemeText :: String}
