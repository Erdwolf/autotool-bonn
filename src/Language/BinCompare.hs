-- -- $Id$

module BinCompare ( bincompare ) where

import Language

bincompare = Language
	   { abbreviation = "{ bin(x) # bin(y) : 0 < x < y }"
	   , alphabet     = mkSet "01#"
	   , sample       = sam 
	   , contains     = con 
	   }

con :: String -> Bool
con w0 = 
    let (x, w1) = span (`elem` "01") w0
	(hash, w2) = span (== '#') w1
	(y, w3) = span (`elem` "01") w2
	lx = length x; ly = length y
    in     take 1 x == "1"
	&& length hash == 1
	&& take 1 y == "1"
	&& null w3
	&& ( lx < ly || ( lx == ly && x < y ) )

sam :: Int -> Int -> IO [ String ]
sam c n | n < 4 = return []
sam c n = 
    let (q, r) = divMod n (length xs)
    in	if 0 == r
	then sequence $ replicate c $ one [ (x, q) | x <- xs ]
	else return []









