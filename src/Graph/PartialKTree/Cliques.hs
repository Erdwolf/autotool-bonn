module Graph.PartialKTree.Cliques where

import Autolib.ToDoc
import Text.XML.HaXml.Haskell2Xml
import Autolib.Reader
import Autolib.Hash

class  ( Ord a , Hash a, Show a, ToDoc a, Reader a, Haskell2Xml a
	 , ToDoc [a], Reader [a] ) => CC a
instance ( Ord a , Hash a, Show a, ToDoc a, Reader a, Haskell2Xml a
	 , ToDoc [a], Reader [a] ) => CC a

-- | liefert maximale Größe einer Clique,
-- die in der Eliminationsordnung verwendet wird
cliques :: CC a
	=> ( Graph a, Int ) 
	-> [ a ]
	-> Reporter Int

cliques (g, k) xs | ist_clique g = do
    let s = cardinality $ knoten g
    inform $ text "verbleibender Graph ist eine Clique der Größe" <+> toDoc s
    assert ( s - 1 <= k ) 
	   $ text "höchstens k+1 Knoten?"
    return $ s - 1

cliques (g, k) ( x : xs ) = do
    peng $ g { show_labels = True , layout_hints = [ "-Nshape=plaintext" ] }
    inform $ text "nächster Knoten:" <+> toDoc x
    let n = nachbarn g x
    inform $ text "hat Nachbarn:" <+> toDoc n
    assert ( cardinality n <= k )
	   $ text "höchstens k Stück?"
    let c = no_fixed_layout $ clique n
        neu = kanten c `minusSet` kanten g
    inform $ text "neue Kanten:" <+> toDoc neu
    let h = union0 c ( restrict ( mkSet xs ) g )
    r <- cliques ( h, k ) xs
    return $ max r ( cardinality n ) 
    

ist_clique :: Ord a => Graph a -> Bool
ist_clique g = 
    let n = cardinality $ knoten g
        m = cardinality $ kanten g
    in  2 * m == n * pred n

no_fixed_layout :: Graph a -> Graph a
no_fixed_layout g = g { graph_layout = emptyFM
		      , layout_hints = []
		      }

