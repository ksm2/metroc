{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MetroLang.Location where

import Data.String

newtype Source = Source String deriving (IsString, Eq, Show, Ord)

data SourceLocation = SourceLocation
  { source :: Source,
    start :: Position,
    end :: Position
  }
  deriving (Show)

data Position = Position
  { line :: Int,
    column :: Int
  }
  deriving (Show)

mkSourceLocation :: Position -> Source -> Int -> SourceLocation
mkSourceLocation p@(Position line col) source len = SourceLocation source p (Position line (col + len))

startPos :: Position
startPos = Position 1 1
