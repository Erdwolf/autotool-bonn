module Graph.Bi.Plain where

--  $Id$

import Graph.Util
import Graph.Bi.Proof

import Autolib.Dot ( peng, Layout_Program (..) )

import Inter.Types
import Autolib.Set
import qualified Challenger as C
import qualified Autolib.Reporter.Set
import Data.Typeable

data Bi = Bi deriving ( Eq, Ord, Show, Read, Typeable )

instance ( GraphC a, Show a )
    => C.Partial Bi (Graph a) (Set a) where

    report _ g = do
        inform $ vcat
	       [ text "Gesucht ist eine Knotenmenge M, die beweist, daß der Graph"
	       , nest 4 $ toDoc g
	       ]
	peng $ g { layout_program = Circo
		 , layout_hints = [ "-Nshape=ellipse" , "-Gsize=\"5,5\"" ]
		 }
        inform $ vcat
	       [ text "bipartit ist, also jede Kante des Graphen zwischen Knoten aus M und V(G)\\M verläuft."
	       ]

    initial _ g = let n = cardinality $ knoten g
                      m = div n 2
                      vs = teilmengen m (knoten g)
		  in case filter (not . is_bi_proof g) vs of [] -> head vs
						             (v:_) -> v

    partial _ g v = do
        let s1 = ( text "Knotenmenge V(G) des Graphen" , knoten g )
	    s2 = ( text "Knotenmenge M in Ihrer Lösung" , v )
	Autolib.Reporter.Set.non_empty s2
	Autolib.Reporter.Set.proper_subset s2 s1
    
    total _ g v = do
        let v_quer  = minusSet (knoten g) v
        let es_norm = select (kanten g) v
	let es_quer = select (kanten g) v_quer

        inform $ vcat [ text "Das Komplement V(G)\\M Ihrer Lösung ist:" 
		      , nest 4 $ toDoc v_quer
		      ]

        when ( not $ isEmptySet es_norm ) $ reject $ vcat
	     [ text "Diese Kante(n) verlaufen zwischen Knoten aus Ihrer Lösung:"
	     , nest 4 $ toDoc es_norm
	     ]

        inform $ vcat [ text "Zwischen den Knoten Ihrer Lösung verlaufen keine Kanten." ]

        when ( not $ isEmptySet es_quer ) $ reject $ vcat
	     [ text "Diese Kante(n) verlaufen zwischen Knoten aus dem Komplement Ihrer Lösung:"
	     , nest 4 $ toDoc es_quer
	     ]

        inform $ vcat 
	       [ text "Zwischen den Knoten des Komplements Ihrer Lösung verlaufen keine Kanten." ]

instance ( GraphC a, Show a )
    => C.Measure Bi (Graph a) (Set a) where
    measure _ _ = fromIntegral . cardinality

-------------------------------------------------------------------------------

make :: Make
make = direct Bi $ mkGraph (mkSet ns) (mkSet es)
    where ns = [ 1..9 ] :: [Int]
	  es = do i <- [1..8] ; return $ kante i 9 :: [Kante Int]
