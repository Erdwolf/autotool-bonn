{-# LANGUAGE ScopedTypeVariables #-}

module Hilbert.BFS where

import Data.Set 

bfs :: Ord a 
    => (a -> Set a )
    -> a
    -> [a]
bfs next start = 
    let result = uniq $ start : ( do x <- result ; toList $ next x )
    in  result

weighted_search :: ( Ord a , Ord w )
    => ( a -> w )
    -> (a -> Set a )
    -> a
    -> [a]
weighted_search weight next start = 
    let -- lift :: a  -> ( w, a )
        lift t = ( weight t, t )
	-- fun :: Set ( w, a ) -> Set a -> [ a ]
        fun todo done = case minView todo of
            Nothing -> []
	    Just (  (w, t) , odo ) -> 
	        if member t done
		then fun odo done
		else let ns = Data.Set.map lift $ next t \\ done
		     in  t : fun ( union odo ns ) ( union done $ singleton t )
    in  fun ( singleton ( lift start ) ) empty

uniq :: Ord a 
     => [a] -> [a]
uniq xs = 
    let f seen [] = []
	f seen (x : xs) = 
	    if x `member` seen
	    then f seen xs
	    else x : f ( union seen $ singleton x ) xs
    in  f empty xs



    