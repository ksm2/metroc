module Commands.Version (getVersion, printVersion) where

import Data.Version (versionBranch)
import Paths_metroc (version)

joinDot :: (Show a) => [a] -> String
joinDot [] = ""
joinDot [element] = show element
joinDot (x : xs) = show x ++ "." ++ joinDot xs

getVersion :: String
getVersion = (joinDot . versionBranch) version

printVersion :: [String] -> IO ()
printVersion _ = putStr "v" >> putStrLn getVersion
