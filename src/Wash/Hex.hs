module Wash.Hex where

import Array
import Char

hexdigit i = hexdigits ! i

hexdigits' = "0123456789ABCDEF"

hexdigits'_indices = [(i, hexdigits'!!i) | i <- [0..15]]

hexdigits = array (0, 15) hexdigits'_indices

fromHexdigits = array (chr 0, chr 255) (map (\ (x,y) -> (y, x)) hexdigits'_indices)

showHex2 ox = 
  [hexdigits ! (ox `div` 16), hexdigits ! (ox `mod` 16)]

hexDigitVal x = fromHexdigits ! x
{-
hexDigitVal x = let (ltDigits, gtDigits) = span (/= x) hexdigits' in
		length ltDigits
-}
