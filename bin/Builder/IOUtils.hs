module Builder.IOUtils (listDirectoryRecursive, matchGlobs) where

import Control.Monad
import System.Directory
import System.FilePath.Posix
import System.FilePattern

flatten :: [[a]] -> [a]
flatten xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []

listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive fp =
  do
    relativeDir <- listDirectory fp
    dir <- return $ map (fp </>) $ relativeDir
    subdirs <- filterM doesDirectoryExist dir
    files <- return $ filter (\x -> notElem x subdirs) dir
    filesInSubdirs <- mapM listDirectoryRecursive subdirs
    return $ files ++ flatten filesInSubdirs

matchGlobs :: [String] -> [String] -> [String]
matchGlobs globs = filter $ \path -> (any (\glob -> glob ?== path) globs)
