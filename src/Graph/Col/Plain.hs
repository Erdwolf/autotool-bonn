module Graph.Col.Plain where

--  $Id$

import Graph.Util
import Graph.Color

import Autolib.Graph.Ops ( gmap )
import Autolib.Graph.Kneser ( petersen )
import Autolib.Dot ( peng, Layout_Program (..) )

import qualified Autolib.Multilingual as M
import qualified Text.PrettyPrint.HughesPJ as T

import Inter.Types
import Autolib.ToDoc
import Autolib.Hash
import Autolib.Size
import Autolib.FiniteMap
import qualified Challenger as C

import qualified Autolib.Reporter.Set
import Data.Typeable

data Col = Col deriving ( Eq, Ord, Show, Read, Typeable )

instance OrderScore Col where
    scoringOrder _ = Increasing

instance ( GraphC a, Show a )
    => C.Partial Col ( Integer, Graph a ) ( FiniteMap a Color ) where

    report p (c, g) = do
        inform $ vcat
	    [ M.make [ (M.DE, T.text "Gesucht ist eine konfliktfreie Knoten-Färbung des Graphen")
                     , (M.UK, T.text "Give a conflict free colouring of")
                     ]
	    , nest 4 $ toDoc g
	    ]
        peng $ g { layout_program = Dot
		 , layout_hints = [ "-Nshape=ellipse" ]
		 }
	inform $ fsep 
	    [ M.make [ ( M.DE, T.text "mit höchstens" )
                     , ( M.UK, T.text "with at most" )  
                     ]
            , toDoc c
            , M.make [ ( M.DE, T.text "verschiedenen Farben." ) 
                     , ( M.UK, T.text "different colours." )
                     ]
            ]

    initial p (c, g) = listToFM $ do
        v <- lknoten g
        let col = toEnum $ fromIntegral $ hash v `mod` 3
	return ( v, col )

    partial p (c, g) f = do
        let s1 = ( M.make [ ( M.DE, T.text "Knotenmenge des Graphen" )
                          , ( M.UK, T.text "node set of graph" )
                          ]
                 , knoten g 
                 )
	    s2 = ( M.make [ ( M.DE, T.text "gefärbte Knoten" )
                          , ( M.UK, T.text "coloured nodes" )
                          ]
                 , mkSet $ keysFM f 
                 )
	Autolib.Reporter.Set.subeq s1 s2
    
    total p (c, g) f = do
        let col v = lookupWithDefaultFM f (error $ "Graph.Col.Plain" ++ show v) v
        let fg = gmap ( \ v ->  (v, col v ) ) g 
        inform $ vcat 
               [ M.make [ ( M.DE, T.text "Der gefärbte Graph ist" )
                        , ( M.UK, T.text "The coloured graph is" )
                        ]
               ]
        peng $ fg { layout_program = Dot
		  , layout_hints = [ "-Nshape=ellipse" ]
		  }
	let wrong = do
	      k <- lkanten g
	      guard $ col (von k) == col (nach k)
	      return k
        when ( not $ null wrong ) $ reject $ vcat
	     [ M.make [ (M.DE, T.text "Diese Kante(n) verlaufen zwischen gleichfarbigen Knoten:" )
                      , (M.UK, T.text "These edge(s) connect nodes of equal colour:")
                      ]
	     , nest 4 $ toDoc wrong
	     ]
	inform $ M.make [ ( M.DE, T.text "Die Färbung ist konfliktfrei." )
                        , ( M.UK, T.text "The colouring is free of conflicts." )
                        ]
        let cc = C.measure p (c, g) f 
	inform $ M.make [ (M.DE, T.text "Sie benutzt"), (M.UK, T.text "it uses" ) ]
		 <+> toDoc cc
		 <+> M.make [ (M.DE, T.text "Farben.") , (M.UK, T.text "colours.") ]
	when ( cc > c ) $ reject 
             $ M.make [ ( M.DE, T.text "erlaubt sind aber höchstens" )
                      , ( M.UK, T.text "but the largest number allowed is")
                      ]
             <+> toDoc c

instance ( GraphC a, Show a )
    => C.Measure Col ( Integer, Graph a ) ( FiniteMap a Color ) where
    measure p i f = fromIntegral $ cardinality $ mkSet $ eltsFM f 

make :: Make
make = direct Col ( 3 :: Integer, petersen )



