module Language.AmBnCmDn (ambncmdn) where

-- -- $Id$

import Language
import Autolib.Set
import Control.Monad ( guard )

ambncmdn :: Language
ambncmdn = l where 
    l = Language 
       { abbreviation = "{ a^m b^n c^m d^n : n, m >= 0 }" 
       , alphabet     = mkSet "abcd"
       , sample       = sam
       , anti_sample  = sample $ komplement l
       , contains     = con
       }

sam :: Int -> Int -> IO [ String ]
sam 0 s = return [ ]
sam c s = return $ take c $ do
    let (mn, r) = s `divMod` 2
    guard $ 0 == r
    m <- [ 0 .. mn ]; let n = mn - m
    return $  replicate m 'a' ++ replicate n 'b'
	   ++ replicate m 'c' ++ replicate n 'd'

con :: String -> Bool
con w0 = 
    let (as, w1) = span (== 'a') w0
	(bs, w2) = span (== 'b') w1
	(cs, w3) = span (== 'c') w2
	(ds, w4) = span (== 'd') w3
    in	null w4 && length as == length cs && length bs == length ds









