module MetroLang.Lang.Error where

import MetroLang.Lang.Lexeme
import MetroLang.Location

data MetroError
  = LexerError SourceLocation Char
  | ParserError Lexeme
