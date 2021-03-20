module MetroLang.Lang.Lexeme where

import MetroLang.Lang.Token
import MetroLang.Location

data Lexeme = L SourceLocation Token String
