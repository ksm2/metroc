module MetroLang.Bytes where

import Numeric (readHex, showHex)
import Data.ByteString
import Data.Serialize
import Data.Word

type Bytes = [Word8]

fromString :: String -> Bytes
fromString [] = []
fromString ('\\':a:b:xs) =
  let [(i, _)] = readHex [a, b]
      c = toEnum i
  in  preChar c (fromString xs)
fromString (x:xs) = preChar x (fromString xs)

toString :: Bytes -> String
toString = enquote . toStringUnquoted

enquote :: String -> String
enquote str = "\"" ++ str ++ "\""

toStringUnquoted :: Bytes -> String
toStringUnquoted [] = ""
toStringUnquoted (x:xs) =
  if (x >= 32 && x <= 127) then (toEnum (fromIntegral x)):(toStringUnquoted xs) else "\\" ++ (strPadLeft '0' 2 (showHex x "")) ++ toStringUnquoted xs

strPadLeft :: Char -> Int -> String -> String
strPadLeft c i str = if (Prelude.length str) < i then strPadLeft c i (c:str) else str

addBytes :: Bytes -> Bytes -> Bytes
addBytes b2 b1 = b1 ++ b2

preChar :: Char -> Bytes -> Bytes
preChar c b = (fromIntegral (fromEnum c)) : b

addChar :: Char -> Bytes -> Bytes
addChar c b = b ++ [(fromIntegral (fromEnum c))]

addInt32 :: Word32 -> Bytes -> Bytes
addInt32 i = addBytes (unpack (encode (byteSwap32 i)))

addString :: String -> Bytes -> Bytes
addString [] b = b
addString (x:xs) b = addString xs $ addChar x b
