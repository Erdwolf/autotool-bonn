--- -- $Id$

module Language.Power 

( power
, nopower
, is_power
)

where

import Language.Type

import Autolib.Set
import Autolib.Util.Zufall

import Data.List (intersperse)


power :: String -> Int -> Language
power sigma k 
      = Language
	{ nametag = "Power" ++ show k
	, abbreviation = foldl1 (++) 
	               [ "{ w^"
		       , show k
		       , " : w in {"
		       , intersperse ',' sigma
		       , "}^* }"
		       ]
	, alphabet     = mkSet sigma
	, contains     = is_power k
	, sample       = sam sigma k
	, anti_sample  = sample ( nopower sigma k )
	}


nopower :: String -> Int -> Language
nopower sigma k = 
        Language
	{ nametag      = "Com" ++ nametag ( power sigma k )
	, abbreviation = foldl1 (++) 
	               [ "Komplement von { w^"
		       , show k
		       , " : w in {"
		       , intersperse ',' sigma
		       , "}^* }"
		       ]
	, alphabet     = mkSet sigma
	, contains     = not . is_power k
	, sample       = random_sample ( nopower sigma k )
	, anti_sample  = sample ( power sigma k )
	}

-------------------------------------------------------------------------

sam :: RandomC m
    => String -> Int -> Int -> Int -> m [ String ]
sam sigma k c n = do
    let q = div n k
    ws <- sequence $ replicate c $ do
	     w <- someIO sigma q
	     return $ concat $ replicate k w
    let w = do _ <- [1 .. k]; r <- sigma; replicate q r
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
splits _ [] = []
splits d xs = 
       let (pre, post) = splitAt d xs
       in  pre :  splits d post












