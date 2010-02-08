module Graph.PartialKTree.Central where

--  $Id$

import Graph.PartialKTree.Cliques
import Graph.PartialKTree.Config
import Graph.PartialKTree.Quiz

import Graph.Util
import Autolib.Graph
import Autolib.Dot ( peng )
import Autolib.Graph.Basic
import Autolib.Graph.Ops

import Inter.Types
import Inter.Quiz
import Autolib.Size
import Autolib.Set
import qualified Challenger as C

import Data.Typeable
import Data.Maybe 

data PartialKTree = PartialKTree deriving ( Eq, Ord, Show, Read, Typeable )

instance OrderScore PartialKTree where
    scoringOrder _ = Increasing

instance CC a
    => C.Measure PartialKTree ( Graph a, Int ) [ a ] where
    measure p ( g, k ) xs = fromIntegral 
			  $ fromMaybe (error "measure")
			  $ result $ cliques (g, k) xs

instance CC a 
    => C.Partial PartialKTree ( Graph a, Int ) [ a ] where

    report p (g, k) = do
	inform $ vcat
	       [ text "Gesucht ist eine Eliminationsordung"
	       , text "mit Weite <=" <+> toDoc k 
	       , text "für diesen Graphen:"
	       , nest 4 $ toDoc g
	       ]
	peng g

    initial p (g, k) = lknoten g

    partial p (g, k) xs = do
        -- auch partielle lösungen sollen kommentiert werden
        let rest = knoten g `minusSet` mkSet xs
	ys <- if isEmptySet rest
	      then return []
	      else do 
		 inform $ vcat 
		        [ text "Diese Knoten des Graphen fehlen in Ihrer Liste"
			, text "und werden deswegen am Ende angefügt:"
			, nest 4 $ toDoc rest
			]
		 return $ setToList rest
	cliques (polish g, k) ( xs ++ ys )
	return ()

    total p (g, k) xs = do
        assert ( mkSet xs == knoten g )
	       $ text "kommen alle Knoten vor?"
	return ()

polish g = g { show_labels = True
	     , layout_program = Dot
	     , layout_hints = [ "-Nshape=plaintext" ]
	     }

------------------------------------------------------------------

make :: Make
make = 
    let k = 4 :: Int
        p = Autolib.Graph.Basic.path [ 1 .. k ]
    in  direct PartialKTree ( normalize $ grid p p, k )

------------------------------------------------------------------

instance Generator PartialKTree Config ( (Graph Int, Int), [ Int ] ) where 
    generator p conf key = do
        ( g, scheme ) <- roll ( width conf ) [ 1 .. nodes conf ]
	g <- remove_kanten (nodes conf `div` 2) g
	return (( polish g, width conf ), scheme )

instance Project PartialKTree ( (Graph Int, Int), [ Int ] ) ( Graph Int, Int ) where
    project p (( g, k ) , scheme) = (g, k)

qmake :: Make
qmake = quiz PartialKTree example

    