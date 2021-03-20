module MetroLang.Utils.Int where

parseHex :: String -> Int
parseHex = parseInt 16

parseOct :: String -> Int
parseOct = parseInt 8

parseBin :: String -> Int
parseBin = parseInt 2

parseInt :: Int -> String -> Int
parseInt _  "" = 0
parseInt _  "0" = 0
parseInt _  [c] = parseIntChar c
parseInt base cs = parseIntChar (last cs) + base * parseInt base (init cs)

parseIntChar :: Char -> Int
parseIntChar '0' = 0
parseIntChar '1' = 1
parseIntChar '2' = 2
parseIntChar '3' = 3
parseIntChar '4' = 4
parseIntChar '5' = 5
parseIntChar '6' = 6
parseIntChar '7' = 7
parseIntChar '8' = 8
parseIntChar '9' = 9
parseIntChar 'a' = 10
parseIntChar 'A' = 10
parseIntChar 'b' = 11
parseIntChar 'B' = 11
parseIntChar 'c' = 12
parseIntChar 'C' = 12
parseIntChar 'd' = 13
parseIntChar 'D' = 13
parseIntChar 'e' = 14
parseIntChar 'E' = 14
parseIntChar 'f' = 15
parseIntChar 'F' = 15
parseIntChar x = error $ "Illegal int char: " ++ [x]

