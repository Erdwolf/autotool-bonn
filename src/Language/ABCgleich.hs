-- -- $Id$

module Gleich 

( gleich 
) 

where


import Language
import Set
import FiniteMap
import Monad ( guard )


gleich :: String -> Language
gleich xs = Language 
       { abbreviation = "{ w : " ++ concat ( intersperse " = "
			[ "|w|_" ++ [x] | x <- xs ] ) ++ " }"
       , alphabet     = mkSet xs
       , sample       = sam xs
       , contains     = con xs
       }

one [] = return []
one xns = do
    let l = length xns
    i <- randomRIO (0, l-1)
    let (pre, (x,n) : post) = splitAt i xns
    let xns' = pre ++ [ (x, n-1) | n > 0 ] ++ post
    w <- one xns'
    return $ x : w

sam :: String -> Int -> Int -> IO [ String ]
sam xs c n = 
    let (q, r) = divMod n (length xs)
    in	if 0 == r
	then sequence $ replicate c $ one [ (x, q) | x <- xs ]
	else return []

con :: String -> String -> Bool
con xs w = 
    let count x = length ( filter (== x) w )
	c : cs = map count xs
    in	all (== c) cs








