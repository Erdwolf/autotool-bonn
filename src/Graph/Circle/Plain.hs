{-# LANGUAGE DeriveDataTypeable #-}
module Graph.Circle.Plain where

--  $Id$

import Graph.Util

import Autolib.Dot ( peng, Layout_Program (..) )

import Inter.Types
import Autolib.Set
import Autolib.Graph.Util ( grad , isZusammen , knotenliste )
import Autolib.Graph.Ops ( restrict )

import qualified Challenger as C

import qualified Autolib.Reporter.Set
import Data.Typeable

data Circle = Circle deriving ( Eq, Ord, Show, Read, Typeable )

instance OrderScore Circle where
    scoringOrder _ = Decreasing

instance ( GraphC a, Show a )
    => C.Partial Circle (Int,Graph a) (Set a) where

    report _ (n,g) = do
        inform $ vcat
	       [ text "Gesucht ist eine Knotenmenge M derart, dass für den Graphen"
	       , nest 4 $ toDoc g
	       ]
	peng $ g { layout_program = Dot
		 , layout_hints = [ "-Nshape=ellipse" , "-Gsize=\"5,5\"" ]
		 }

        inform $ vcat
	       [ text "die Beschränkung R(G,M) von G auf M ein Kreis der Größe"
	       , nest 4 $ toDoc n
	       , text "ist."
	       ]

    initial _ (_,g) = 
        let zweier = sfilter ( ( == 2 ) . grad g ) (knoten g)
            rest = minusSet (knoten g) zweier
            z = div (cardinality zweier) 3
	    r = div (cardinality rest) 2
        in union (head $ teilmengen z  zweier) (head $ teilmengen r rest)

    partial _ (n,g) v = do
        when (cardinality v /= n) $ reject $ vcat
	     [ text "Sie haben"
	     , nest 4 $ toDoc $ cardinality v
	     , text "Knoten angegeben, es sollen aber"
	     , nest 4 $ toDoc n
	     , text "Knoten sein."
	     ]

        inform $ vcat [ text "Es handelt sich um"
		      , nest 4 $ toDoc n
		      , text "Knoten."
		      ]

        let s1 = ( text "Knotenmenge V(G) des Graphen" , knoten g )
	    s2 = ( text "Knotenmenge M in Ihrer Lösung" , v )

	Autolib.Reporter.Set.subeq s2 s1
    
    total _ (_,g) v = do
        let c = restrict v g

        inform $ vcat 
	       [ text "Ihre Knotenmenge lautet:"
	       , nest 4 $ toDoc v
	       , text "Die Beschränkung R(G,M) von G auf M"
	       , nest 4 $ toDoc c
	       , text "hat die Gestalt"
	       ]

        peng $ c { layout_program = Circo
		 , layout_hints = [ "-Nshape=ellipse" , "-Gsize=\"5,5\"" ]
		 }

        when ( not $ isZusammen c ) $ reject $ vcat
	     [ text "R(G,M) ist nicht zusammenhängend." ]

        inform $ vcat [ text "R(G,M) ist zusammenhängend." ]

        let grade = map (grad c) (knotenliste c)

        when ( any ( /= 2 ) grade ) $ reject $ vcat
	     [ text "Die Knoten"
	     , nest 4 $ toDoc $ filter ( (/=2) . grad c ) (knotenliste c)
	     , text "von R(G,M) haben nicht den Grad 2."
	     ]

        inform $ vcat [ text "Alle Knoten von R(G,M) haben den Grad 2." ]

instance ( GraphC a, Show a )
    => C.Measure Circle (Int,Graph a) (Set a) where
    measure _ _ = fromIntegral . cardinality

make :: Make
make = direct Circle 
     ( 6 :: Int
     , mkGraph (mkSet [(1::Int)..12])
     $ mkSet ([ kante 1 2, kante 1 6, kante 1 7, kante 1 8
	      , kante 1 10, kante 1 11, kante 2 5, kante 2 9, kante 3 6
	      , kante 3 7, kante 3 9, kante 3 10, kante 3 12, kante 4 9
	      , kante 4 12, kante 5 8, kante 5 10, kante 5 12, kante 6 8
	      , kante 6 9, kante 7 8, kante 7 9, kante 8 9, kante 8 11
	      , kante 9 11, kante 10 12, kante 11 12
	      ] :: [Kante Int]
	     )
     )

