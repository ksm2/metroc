module MetroLang.Bytes where

import qualified Codec.Binary.UTF8.String as UTF8
import Data.ByteString (unpack)
import Data.Serialize
import Data.Word
import Numeric (readHex, showHex)

type Bytes = [Word8]

fromString :: String -> Bytes
fromString [] = []
fromString ('\\' : a : b : xs) =
  let [(i, _)] = readHex [a, b]
      c = toEnum i
   in (UTF8.encodeChar c) ++ (fromString xs)
fromString (x : xs) = (UTF8.encodeChar x) ++ (fromString xs)

toWasmStringLiteral :: Bytes -> String
toWasmStringLiteral = enquote . toWasmStringUnquoted

enquote :: String -> String
enquote str = "\"" ++ str ++ "\""

toWasmStringUnquoted :: Bytes -> String
toWasmStringUnquoted = (foldl (++) "") . (map encodeByte)

encodeByte :: Word8 -> String
encodeByte c
  | c == 0x09 = "\\t"
  | c == 0x0A = "\\n"
  | c == 0x0D = "\\r"
  | c == 0x22 = "\\\""
  | c == 0x27 = "\\'"
  | c == 0x5C = "\\\\"
  | c >= 0x20 && c /= 0x7F = [toEnum (fromIntegral c)]
  | otherwise = "\\" ++ (strPadLeft '0' 2 (showHex c ""))

strPadLeft :: Char -> Int -> String -> String
strPadLeft c i str = if (Prelude.length str) < i then strPadLeft c i (c : str) else str

int32ToBytes :: Int -> Bytes
int32ToBytes i = unpack (encode (byteSwap32 (fromIntegral i)))

stringToBytes :: String -> Bytes
stringToBytes = UTF8.encode

utf8Length :: String -> Int
utf8Length = length . UTF8.encode
