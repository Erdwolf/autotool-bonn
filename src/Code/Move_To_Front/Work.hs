module Code.Move_To_Front.Work where

--  $Id$

import Autolib.Set
import Autolib.Util.Splits

import Data.List
import Control.Monad
import Code.Move_To_Front.Data



-- | assumes that alphabet consists of exactly the set of letters 
-- in the string that is to be encoded
encode :: Ord a 
       => [ a ] 
       -> Coding [a]
encode xs = 
    let work q x = head $ do
	    (pre , y : post) <- zip (inits q) (tails q)
	    guard $ x == y
	    return ( y : pre ++ post , length pre )
	start = setToList $ mkSet xs
	( end, out ) = mapAccumL work start xs
    in  Coding { queue = start
	       , output = out
	       }

decode :: Ord a 
       => Coding [a]
       -> [a]
decode c = 
    let 
        work q k = 
	    let ( pre, x : post ) = splitAt k q
	    in  ( x : pre ++ post , x )
    	( end, out ) = mapAccumL work (queue c) (output c)
    in	out

