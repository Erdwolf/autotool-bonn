module Graph.PartialKTree.Quiz where

--  $Id$

import Graph.Util
import Autolib.Graph.Type
import Autolib.Graph.Ops
import Autolib.Graph.Basic
import Autolib.Set

import System.Random
import Autolib.Util.Zufall

roll :: Ord a 
     => Int -- ^ intended tree width (upper bound)
     -> [ a ] -- ^ nodes
     -> IO ( Graph a, [a] )
roll k xs | length xs <= k = do
    let h = no_fixed_layout $ clique $ mkSet xs
    return ( h, lknoten h )

roll k xs = do
    x : ys <- permutation xs
    ( g, scheme ) <- roll k ys
    c <- eins $ filter ( (<= k) . cardinality ) $ cliquen g
    let ks = do y <- setToList c ; return kante x y
    let h = clique [x] `union0` g
    return ( links h ks , x : scheme )

cliquen g = 
    let cls [] = return emptySet
	cls (x : xs) = cls xs ++ do
	    c <- cls $ nachbarn g x 
	    return $ unitSet x `Autolib.Set.union` c
    in  cls $ lknoten g


