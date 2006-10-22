module Graph.Bi.Proof where

--  $Id$

import Autolib.Graph.Graph
import Autolib.Set

----------------------------------------------------------------------------------------------------

is_bi_proof :: Ord a => Graph a -> Set a -> Bool
is_bi_proof g v = and [ not $ isEmptySet v
		      , not $ subseteq (knoten g) v
		      , subseteq v (knoten g)
		      , isEmptySet $ select (kanten g) v
		      , isEmptySet $ select (kanten g) $ minusSet (knoten g) v
		      ]

select :: Ord a => Set (Kante a) -> Set a -> Set (Kante a)
select es v = intersect es (smap (uncurry Kante) $ cross v v)
