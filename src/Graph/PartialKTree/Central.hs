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

    partial p (g, k) xs = 
        assert ( mkSet xs == knoten g )
	       $ text "kommen alle Knoten vor?"

    total p (g, k) xs = do
	cliques (g, k) xs
	return ()

------------------------------------------------------------------

make :: Make
make = 
    let k = 4 :: Int
        p = path [ 1 .. k ]
    in  direct PartialKTree ( normalize $ grid p p, k )

------------------------------------------------------------------

instance Generator PartialKTree Config ( (Graph Int, Int), [ Int ] ) where 
    generator p conf key = do
        ( g, scheme ) <- roll ( width conf ) [ 1 .. nodes conf ]
	g <- remove_kanten (nodes conf `div` 2) g
	let h = g { show_labels = True
		  , layout_program = Dot
		  , layout_hints = [ ]
		  }
	return (( h, width conf ), scheme )

instance Project PartialKTree ( (Graph Int, Int), [ Int ] ) ( Graph Int, Int ) where
    project p (( g, k ) , scheme) = (g, k)

qmake :: Make
qmake = quiz PartialKTree example

    