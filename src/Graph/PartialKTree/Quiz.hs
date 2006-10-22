module Graph.PartialKTree.Quiz where

--  $Id$

import Graph.Util
import Autolib.Graph.Type
import Autolib.Graph.Ops
import Autolib.Graph.Basic
import Autolib.Set

import System.Random
import Autolib.Util.Zufall

-- | entfernt eine kante
-- von Knoten mit Grad > 2
remove_kante :: ( ToDoc [a], Ord a ) 
	      => Graph a -> IO ( Graph a )
remove_kante g = do
    let thick = sfilter ( \ v -> degree g v > 2 ) $ knoten g
    if isEmptySet thick 
       then return g
       else do
	    k <- eins $ lkanten $ restrict thick g
	    return $ unlink g k

remove_kanten :: ( ToDoc [a], Ord a ) 
	      => Int -> Graph a -> IO ( Graph a )
remove_kanten 0 g = return g
remove_kanten k g = do
    h <- remove_kante g
    remove_kanten (k-1) h


-- | erzeuge vollständigen k-tree, mit perf. elim. scheme
roll :: (ToDoc [a], ToDoc a, Ord a)
     => Int -- ^ intended tree width (upper bound)
     -> [ a ] -- ^ nodes
     -> IO ( Graph a, [a] )

roll k xs | length xs <= k = do
    let h = no_fixed_layout $ clique $ mkSet xs
    return ( h, lknoten h )

roll k xs = do
    x : ys <- permutation xs
    ( g, scheme ) <- roll k ys
    let nice c = let s = cardinality c in s == k
    c <- eins $ filter nice $ cliquen g
    let ks = do y <- setToList c ; return $ kante x y
    let h = no_fixed_layout ( clique $ unitSet x ) `union0` g
    return ( links h ks , x : scheme )

cliquen :: ( ToDoc [a], Ord a ) => Graph a -> [ Set a ]
cliquen g | isEmptySet ( knoten g ) = [ emptySet ]
cliquen g = 
    let x : xs = lknoten g 
    in  -- Fall 1: x ist nicht in der Clique
           cliquen ( restrict (mkSet xs) g )
	-- Fall 2: x ist doch in der Clique
	++ do  c <- cliquen $ restrict (nachbarn g x) g
	       return $ unitSet x `Autolib.Set.union` c



