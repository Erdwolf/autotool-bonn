--- $Id$

module Language.Power 

( power
, nopower
, is_power
)

where

import Language.Type

import Set
import Util.Wort
import List (nub)


power :: Int -> Language
power k = Language
	{ abbreviation = "{ w^" ++ show k ++ " : w in {0, 1}^* }"
	, alphabet     = mkSet "01"
	, contains     = is_power k
	, sample       = sam k
	}


nopower :: Int -> Language
nopower k = Language
	{ abbreviation = "Komplement von { w^" ++ show k ++ " : w in {0,1}^* }"
	, alphabet     = mkSet "01"
	, contains     = not . is_power k
	, sample       = random_sample ( nopower k )
	}

-------------------------------------------------------------------------

sam :: Int -> Int -> Int -> IO [ String ]
sam k c n = do
    let (q, r) = divMod n k
    ws <- sequence $ replicate c $ do
	     w <- someIO "01" q
	     return $ concat $ replicate k w
    let w = do c <- [1 .. k]; r <- "01"; replicate n r
    return $ w : ws

----------------------------------------------------------------------------

is_power :: Eq a => Int -> [a] -> Bool
is_power expo w = 
             let n = length w
		 (q, r) = divMod n expo
		 (p : ps) = splits q w
	     in	 null w 
		 || ( (0 == r) &&  all (== p) ps )
	       
splits :: Int -> [a] -> [[a]]
splits d [] = []
splits d xs = 
       let (pre, post) = splitAt d xs
       in  pre :  splits d post












