{-# LANGUAGE DeriveDataTypeable #-}
module Graph.TreeWidth where

--  $Id$

import Graph.Util
import Autolib.Graph
import Autolib.Dot ( peng )
import Autolib.Graph.Basic
import Autolib.Graph.Ops

import Inter.Types
import Autolib.ToDoc
import Autolib.Size
import Autolib.Set
import Autolib.FiniteMap
import qualified Challenger as C

import Data.Typeable
import Data.List ( inits, tails )

data TreeWidth = TreeWidth deriving ( Eq, Ord, Show, Read, Typeable )

instance OrderScore TreeWidth where
    scoringOrder _ = Increasing

instance C.Measure TreeWidth ( Graph Int, Int ) 
    ( FiniteMap Int (Set Int)    , Graph Int ) where
    measure p ( g, w ) ( fm, t ) = fromIntegral $
        maximum $ do ( i, s ) <- fmToList fm ; return $ cardinality s

instance C.Partial TreeWidth ( Graph Int, Int ) 
    ( FiniteMap Int (Set Int)    , Graph Int ) where

    report p (g, w) = do
	inform $ vcat
	       [ text "Gesucht ist eine Baum-Überdeckung"
	       , text "mit Weite <=" <+> toDoc w 
	       , text "für diesen Graphen:"
	       , nest 4 $ toDoc g
	       ]
	peng g

    initial p (g, w) = 
        let c = do
	      vs <- tails $ lknoten g
	      let s = mkSet $ take (succ w) vs
	      guard $ w < cardinality s
	      return s
            v = [ 1 .. length c ]  
	in  ( listToFM $ zip v c , Autolib.Graph.Basic.path v )

    partial p (g, w) ( fm, t ) = do

        inform $ text "Die Struktur Ihrer Überdeckung ist:"
	-- setToList, damit die Beschriftung kleiner wird (sonst mkSet)
        peng $ gmap ( \ i -> setToList $ lookupset fm i ) t

	assert ( is_connected t 
		 && cardinality (kanten t) < cardinality (knoten g) )
	       ( text "Ist das ein Baum?" )

        inform $ text "Ist keine Menge zu groß?"
        let large = do 
	      (i, s) <- fmToList fm
	      guard $ cardinality s - 1 > w
	      return (i, s)
	when ( not $ null large ) $ reject $ vcat
	     [ text "doch, diese:"
	     , nest 4 $ toDoc large
	     ]

    total p (g, w) ( fm, t ) = do

        inform $ text "Ist jeder Knoten überdeckt?"
	let nein = knoten g `minusSet` unionManySets ( eltsFM fm )
        when ( not $ isEmptySet nein ) $ reject $ vcat
	     [ text "nein, diese nicht:"
	     , nest 4 $ toDoc nein
	     ]

        inform $ text "Ist jede Kante überdeckt?"
	-- welcher Knoten in welchen Mengen?
	let wh = addListToFM_C Autolib.Set.union emptyFM $ do
		  (i, s) <- fmToList fm
                  x <- setToList s
		  return ( x, unitSet i )
	let nein = do 
	       k <- lkanten g
	       let xs = lookupset wh $ von k
		   ys = lookupset wh $ nach k
	       guard $ isEmptySet $ intersect xs ys
	       return (k, xs, ys)
        when ( not $ null nein ) $ reject $ vcat
	     [ text "nein, diese Kanten nicht:"
	     , nest 4 $ vcat $ do
	          (k, xs, ys) <- nein
	          return $ vcat 
	                 [ toDoc k
			 , nest 4 $ vcat
	                          [ text "die Knoten sind in den Mengen:"
				  , nest 4 $ toDoc xs
				  , nest 4 $ toDoc ys
				  ]
			 ]
	     ]


        inform $ text "Gilt die Helly-Eigenschaft?"
        let nein = do
	      ( x, is ) <- fmToList wh
	      guard $ not $ is_connected $ restrict is t
	      return (x, is)
	when ( not $ null nein ) $ reject $ vcat 
	     [ text "nein:"
	     , nest 4 $ vcat $ take 5 $ do
	          ( x, is ) <- nein
	          return $ vcat 
	                 [ text "Der Knoten" <+> toDoc x
			 , text "ist enthalten in den Mengen" <+> toDoc is
			 , text "aber diese bilden keinen zusammenhängenden Teilgraphen."
			 ]
	     ]

make :: Make
make = 
    let p = Autolib.Graph.Basic.path [ 1 .. 4 :: Int ]
    in  direct TreeWidth ( normalize $ grid p p, 3 :: Int )

