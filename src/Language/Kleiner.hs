-- -- $Id$

module Kleiner

( kleiner
) 

where

import Language
import Set
import FiniteMap
import List (intersperse, nub, sort)
import Random
import Genau

kleiner :: String -> Language
kleiner xs = Language 
       { abbreviation = "{ w : " ++ concat ( intersperse " <= "
			[ "|w|_" ++ [x] | x <- xs ] ) ++ " }"
       , alphabet     = mkSet xs
       , sample       = sam xs
       , contains     = con xs
       }


upseq 1 n lo = return [ n ]
upseq l n lo = do
    let hi = n `div` l
    x  <- randomRIO (lo, hi)
    xs <- upseq (l-1) (n-x) x
    return $ x : xs

sam :: String -> Int -> Int -> IO [ String ]
sam xs c n = do
    let l = length xs
    let (q, r) = divMod n l

    -- hat evtl. größere länge als n
    ys <- genau $ zip xs $ repeat 1
    let w = do y <- ys; replicate (q+1) y

    ws <- do ks <- upseq (length xs) n 0
	     sequence $ replicate c $ genau $ zip xs ks
    return $ w : ws

con :: String -> String -> Bool
con xs w = 
    let count x = length ( filter (== x) w )
	cs = map count xs
    in	and $ zipWith (<=) cs (tail cs)








