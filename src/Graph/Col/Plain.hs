module Graph.Col.Plain where

--  $Id$

import Graph.Util
import Graph.Color

import Autolib.Graph.Ops ( gmap )
import Autolib.Graph.Kneser ( petersen )
import Autolib.Dot ( peng, Layout_Program (..) )

import Inter.Types
import Autolib.ToDoc
import Autolib.Hash
import Autolib.Size
import Autolib.FiniteMap
import qualified Challenger as C

import qualified Autolib.Reporter.Set
import Data.Typeable

data Col = Col deriving ( Eq, Ord, Show, Read, Typeable )

instance ( GraphC a, Show a )
    => C.Partial Col ( Integer, Graph a ) ( FiniteMap a Color ) where

    report p (c, g) = do
        inform $ vcat
	       [ text "Gesucht ist eine konfliktfreie Knoten-Färbung des Graphen"
	       , nest 4 $ toDoc g
	       ]
        peng $ g { layout_program = Dot
		 , layout_hints = [ "-Nshape=ellipse" ]
		 }
	inform $ fsep 
	       [ text "mit höchstens", toDoc c, text "verschiedenen Farben." ]

    initial p (c, g) = listToFM $ do
        v <- lknoten g
        let col = toEnum $ fromIntegral $ hash v `mod` 3
	return ( v, col )

    partial p (c, g) f = do
        let s1 = ( text "Knotenmenge des Graphen" , knoten g )
	    s2 = ( text "gefärbte Knoten" , mkSet $ keysFM f )
	Autolib.Reporter.Set.subeq s1 s2
    
    total p (c, g) f = do
        let col v = lookupWithDefaultFM f (error $ "Graph.Col.Plain" ++ show v) v
        let fg = gmap ( \ v ->  (v, col v ) ) g 
        inform $ vcat [ text "Der gefärbte Graph ist" ]
        peng $ fg { layout_program = Dot
		  , layout_hints = [ "-Nshape=ellipse" ]
		  }
	let wrong = do
	      k <- lkanten g
	      guard $ col (von k) == col (nach k)
	      return k
        when ( not $ null wrong ) $ reject $ vcat
	     [ text "Diese Kante(n) verlaufen zwischen gleichfarbigen Knoten:"
	     , nest 4 $ toDoc wrong
	     ]
	inform $ text "Die Färbung ist konfliktfrei."
        let cc = C.measure p (c, g) f 
	inform $ text "Sie benutzt" 
		 <+> toDoc cc
		 <+> text "Farben."
	when ( cc > c ) $ reject $ text "erlaubt sind aber höchstens" <+> toDoc c

instance ( GraphC a, Show a )
    => C.Measure Col ( Integer, Graph a ) ( FiniteMap a Color ) where
    measure p i f = fromIntegral $ cardinality $ mkSet $ eltsFM f 

make :: Make
make = direct Col ( 3 :: Integer, petersen )



