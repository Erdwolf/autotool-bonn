-- $Header$

module ABCdiff (abcdiff) where

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
import List ( sort, nub )
import Random

abcdiff :: Language
abcdiff = Language 
       { abbreviation = "{ a^i b^j c^k : i /= j, j /= k, k /= i }" 
       , alphabet     = mkSet "abc"
       , sample       = sam
       , contains     = con
       }

sam :: Int -> Int -> IO [ String ]
sam c n = do
    ws <- sequence $ replicate c $ do
	  w <- sequence $ replicate n $ randomRIO ('a', 'c')
	  return $ sort w
    return $ filter (contains abcdiff) $ nub ws

con :: String -> Bool
con w0 = 
    let (as, w1) = span (== 'a') w0; i = length as
	(bs, w2) = span (== 'b') w1; j = length bs
	(cs, w3) = span (== 'c') w2; k = length cs
    in	null w3 && (i /= j) && (j /= k) && (k /= i)








