module MetroLang.Location where

data SourceLocation = SourceLocation
  { source :: String,
    content :: String,
    start :: Position,
    end :: Position
  }
  deriving (Show)

data Position = Position
  { line :: Int,
    column :: Int
  }
  deriving (Show)
