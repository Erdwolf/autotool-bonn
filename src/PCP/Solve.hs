{-# OPTIONS -fallow-overlapping-instances #-}

module PCP.Solve where

import Autolib.Schichten
import Autolib.Util.Hide
import Autolib.Util.Splits
import Autolib.Hash
import Autolib.Sets
import Autolib.ToDoc

import Control.Monad
import Data.Maybe

import PCP.Type
import PCP.Examples

type Conf c = ( Maybe [c], Hide [Int] )


fsolve :: String
	  -> Int
	  -> [[Int]]
fsolve w cc = do
    c <- [ 1 .. cc ]
    (d, sol) <- tree $ fpairs w c
    guard $ null d
    return $ reverse sol




fpairs w c = 
    let inf = [ repeat ("w", "0")
	      , repeat ("0","1")
	      , repeat ("1", "w")
	      ]
        fin = [ replicate c (w, "0")
              , []
	      , replicate c ("1", w)
	      ]
    in inf ++ fin

tree :: (Hash c, Ord c )
     => [[Pair c]]
     -> [([c], [Int])] -- ^ list of (diff, reverse prefix)
tree pairs = tree_from pairs []

tree_from pairs w = do
    ( _, d , _, Hide is ) <- 
	-- bfs 
        dfs
            next ( hash w, w, Hide pairs, Hide [] )
    return ( d, is )

next ( _, diff, Hide pairs, Hide is ) = mkSet $ do
    -- use first pair from some list
    ( ps, ((l,r) : rest) : qs ) <- splits pairs
    let i = length ps
    let (pre, post) = splitAt (length r) (diff ++ l)
    guard $ pre == r
    return ( hash post
	   , post 
	   , Hide $ ps ++ [ rest ] ++ qs 
	   , Hide $ i : is 
	   )

-------------------------------------------------------

dfs :: Ord a => (a -> Set a) -> a -> [a]
dfs next x0 = 
    let dfs' (x : todo) done = x : 
           let now = union done $ unitSet x
               neu = minusSet (next x) now
           in  dfs' (setToList neu ++ todo) now
        dfs' [] done = []
    in  dfs' [x0] emptySet
