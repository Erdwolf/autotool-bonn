-- $Id$

module Graph.Ham where

-- | hamiltonsche wege: wege, die jeden knoten genau einmal berühren

import Autolib.Graph.Type ( Graph , Kante , kante , von , nach , knoten 
			  , mkGraph 
			  )
import Autolib.Graph.Util ( knotenliste )
import Autolib.Set ( Set , mkSet , addToSet , elementOf )
import Autolib.Util.Hamming ( hamming )
import Autolib.Util.Wort ( alle )
import Graph.Util ( lnachbarn )
import Control.Monad ( guard )

-------------------------------------------------------------------------------

-- | ein weg ist als eine (nichtleere) kantenfolge definiert

type Weg a = [Kante a]

-------------------------------------------------------------------------------

-- | jedem weg, der an einem knoten n beginnt, ist eindeutig eine folge
-- | von knoten zugeordnet, die durchlaufen werden

knotenfolge :: Eq a => a -> Weg a -> [a]
knotenfolge n [k]    | von k == n = [n,nach k]
		     | otherwise  = [n,von  k]
knotenfolge n (k:ks) | von k == n = n : knotenfolge (nach k) ks
		     | otherwise  = n : knotenfolge (von  k) ks
knotenfolge _ _ = []

-------------------------------------------------------------------------------

-- | alle hamiltonschen wege als weg und als knotenfolge

alle_hamiltons :: Ord a => Graph a -> [(Weg a,[a])]
alle_hamiltons g = do n <- knotenliste g
		      w <- build [] (mkSet [n]) g n
		      return (w,knotenfolge n w)

-------------------------------------------------------------------------------

build :: Ord a => Weg a -> Set a -> Graph a -> a -> [Weg a]
build ks ns g n
    | ns == knoten g = [ks]
    | otherwise      = concat $ do 
		       m <- lnachbarn g n
		       guard $ not $ elementOf m ns
		       let k = kante n m
		       guard $ not $ elem k ks
		       return $ build (ks ++ [k]) (addToSet ns m) g m

-------------------------------------------------------------------------------

-- | beispielgraphen

a, b, c :: Graph Int
a = mkGraph (mkSet [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ])
	    (mkSet [ kante 1 9, kante 2 9, kante 3 9, kante 4 9
		   , kante 5 9, kante 6 9, kante 7 9, kante 8 9
		   ]
	    )
b = mkGraph (mkSet [ 1, 2, 3, 4, 5, 6, 7 ])
            (mkSet [ kante 1 2, kante 1 3, kante 2 4, kante 2 5
                   , kante 4 6, kante 4 7, kante 5 7
                   ]
	    )
c = mkGraph (mkSet [ 1, 2, 3, 4, 5, 6 ])
	    (mkSet [ kante 1 2, kante 1 4, kante 1 6, kante 2 3
                   , kante 2 5, kante 3 4, kante 3 6, kante 4 5, kante 5 6
                   ]
	     )

-------------------------------------------------------------------------------

-- | kanten zwischen genau den knoten, die hammingdistance 1 haben

hypercube :: Int -> Graph String
hypercube n = let nodes = alle "01" n
	      in mkGraph (mkSet nodes) $ mkSet $ do
	         x <- nodes
		 y <- nodes
		 guard $ hamming x y == 1
		 return $ kante x y

-- | wie hypercube, aber als knoten der wert, wenn die knoten in hypercube
--   als binärziffer aufgefasst werden

hypercubeI :: Int -> Graph Int
hypercubeI n = let nodes        = alle [0,1] n
		   val v (0:xs) = val (v+v  ) xs
		   val v (1:xs) = val (v+v+1) xs
		   val v _      = v
	       in mkGraph (mkSet $ map (val 0) nodes) $ mkSet $ do
	          x <- nodes
		  y <- nodes
		  guard $ hamming x y == 1
		  return $ kante (val 0 x) (val 0 y)
