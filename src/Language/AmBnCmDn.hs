-- $Header$

module AmBnCmDn (ambncmdn) where

-- $Log$
-- Revision 1.1  2002-11-08 16:43:23  joe
-- language -> Language
-- pump -> Pump.REG (neu)
-- aufgaben dazu
--
-- Revision 1.1.1.1  2002/05/24 10:46:48  challenger
-- start
--
-- Revision 1.1  2001/12/05 21:11:31  autotool
-- lang/*
--
-- Revision 1.1  2001/12/04 13:04:10  autotool
-- neu: pump/* lang/*
--

import Language
import Set
import Monad ( guard )

ambncmdn :: Language
ambncmdn = Language 
       { abbreviation = "{ a^m b^n c^m d^n : n, m >= 0 }" 
       , alphabet     = mkSet "abcd"
       , sample       = sam
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









