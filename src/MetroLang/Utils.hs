module MetroLang.Utils where

indent2 :: [String] -> String
indent2 = (prefix "\n") . (indent "  ") . unlines

prefix :: String -> String -> String
prefix pref str = pref ++ str

indent :: String -> String -> String
indent identWith str = unlines $ map (prefix identWith) $ lines str
