module Graph.PartialKTree.Cliques where

--  $Id$

import Graph.Util
import Autolib.Graph.Graph
import Autolib.Graph.Ops
import Autolib.Graph.Basic
import Autolib.Dot
import Text.XML.HaXml.Haskell2Xml
import Autolib.Reader
import Autolib.Hash
import Autolib.ToDoc
import Autolib.Reporter


class  ( Ord a , Hash a, Show a, ToDoc a, Reader a, Haskell2Xml a
	 , ToDoc [a], Reader [a] 
       , GraphC a ) => CC a
instance ( Ord a , Hash a, Show a, ToDoc a, Reader a, Haskell2Xml a
	 , ToDoc [a], Reader [a] 
	 , GraphC a
	 ) => CC a

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
    peng g
    inform $ text "nächster Knoten:" <+> toDoc x
    let n = nachbarn g x
    inform $ text "hat Nachbarn:" <+> toDoc n
    assert ( cardinality n <= k )
	   $ text "höchstens k Stück?"
    let c = no_fixed_layout $ clique n
        neu = kanten c `minusSet` kanten g
    inform $ text "neue Kanten:" <+> toDoc neu
    let h = union0 ( restrict ( mkSet xs ) g ) c
    r <- cliques ( h, k ) xs
    return $ max r ( cardinality n ) 
    


