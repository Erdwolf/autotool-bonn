{-# OPTIONS -fallow-overlapping-instances #-}

module PCP.Solve where

import Autolib.Schichten
import Autolib.Util.Hide
import Autolib.Sets

import Control.Monad
import Data.Maybe

import PCP.Type
import PCP.Examples

type Conf c = ( Maybe [c], Hide [Int] )

solve :: Ord c 
	  => PCP c
	  -> Int
	  -> [[Int]]
solve pcp width = do
    br <- [ 1 .. width ]
    fun <- [ id {- , turn -} ]
    (d, w) <- tree ( fun pcp ) br
    guard $ null d
    return w

merge :: [a] -> [a] -> [a]
merge (x : xs) ys = x : merge ys xs
merge []       ys = ys

tree :: Ord c 
     => PCP c
     -> Int
     -> [([c], [Int])]
tree pcp width = do
    let next ( mdiff, Hide is ) = mkSet $ do
            let diff = fromMaybe [] mdiff

            guard $ length diff < width

            (i, (l, r)) <- zip [0..] pcp
            let (pre, post) = splitAt (length r) (diff ++ l)
            guard $ pre == r
	    return ( Just $ post, Hide $ i : is )
    ( Just d , Hide w ) <- bfs next ( Nothing, Hide [] )
    return ( d, reverse w )



