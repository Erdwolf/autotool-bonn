module Hex (hex) where

import Data.List (unfoldr)
import Data.Word

hex :: Word -> String
hex = map ((['0'..'9']++['A'..'F'])!!) . map wordToInt . base 16

wordToInt :: Word -> Int
wordToInt = fromIntegral

base b n = unfoldr uf n
   where uf 0 = Nothing
         uf x = Just (mod x b, div x b)

