module Builder.IOUtils (listDirectoryRecursive, matchGlobs) where

import Control.Monad
import System.Directory
import System.FilePath.Posix
import System.FilePattern

flatten :: [[a]] -> [a]
flatten xs = (\z n -> foldr (flip (foldr z)) n xs) (:) []

listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive fp =
  do
    relativeDir <- listDirectory fp
    let dir = map (fp </>) relativeDir
    subdirs <- filterM doesDirectoryExist dir
    let files = filter (`notElem` subdirs) dir
    filesInSubdirs <- mapM listDirectoryRecursive subdirs
    return $ files ++ flatten filesInSubdirs

matchGlobs :: [String] -> [String] -> [String]
matchGlobs globs = filter $ \path -> any (?== path) globs
