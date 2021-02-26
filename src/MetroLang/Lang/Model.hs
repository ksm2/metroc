module MetroLang.Lang.Model where

data Module
  = Module [Declaration]
  deriving (Show)

data Declaration
  = ImportDeclaration FQN
  deriving (Show)

type FQN = [String]
