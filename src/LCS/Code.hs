module LCS.Code 

( lcs
, is_embedded_in
)

where

--  $Id$

import Data.Array

data Entry a = 
     Entry { height :: Int
	   , path   :: [a]
	   }
     deriving Show

instance Eq ( Entry a ) where 
    x == y = height x == height y
instance Ord ( Entry a ) where 
    compare x y = compare (height x) (height y)

-- | return a longest common subsequence
-- use greedy algorithm with quadratic space and time
lcs :: Eq a => [a] -> [a] -> [a]
lcs ( xs :: [a] ) ( ys :: [a] ) =
    let lx = pred $ length xs ; ly = pred $ length ys
	top = ( lx, ly )
        bnd =  ((-1 , -1), top)
	a :: Array (Int, Int) ( Entry a )
	a = accumArray ( flip const ) 
		       ( Entry { height = 0, path = [] } )
		       bnd
	  $ do p @ ( i, j ) <- range (( 0,0 ), top)
	       let lift (x, y) e =
		    if x == y 
		    then Entry { height = succ $ height e
			       , path   = x : path e
			       }
		    else e
	       return ( p
		      , maximum [ lift (xs !! i, ys !! j) (a ! (i-1 , j-1))
				, a ! (i-1 , j  ) 
				, a ! (i   , j-1) 
				]
		      )
    in  reverse $ path (a ! top)

-- | check if subsequence relation holds
-- use quadratic space and time
is_embedded_in :: Eq a => [a] -> [a] -> Bool
is_embedded_in xs ys = xs == lcs xs ys

