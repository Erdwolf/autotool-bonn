-- $Header$

module Kleiner

( kleiner
) 

where

-- $Log$
-- Revision 1.1  2002-11-08 16:43:23  joe
-- language -> Language
-- pump -> Pump.REG (neu)
-- aufgaben dazu
--
-- Revision 1.1.1.1  2002/05/24 10:46:48  challenger
-- start
--
-- Revision 1.3  2001/12/12 08:14:13  autotool
-- fix (wg. genau)
--
-- Revision 1.2  2001/12/12 07:59:15  autotool
-- beim harten test nicht die buchstaben verwürfeln
--
-- Revision 1.1  2001/12/10 00:37:33  autotool
-- einige neue sprachen
--
-- Revision 1.1  2001/12/05 21:11:31  autotool
-- lang/*
--

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








