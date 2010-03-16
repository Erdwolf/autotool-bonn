{-# language OverlappingInstances, IncoherentInstances, FlexibleInstances #-}
{-# language MultiParamTypeClasses, DeriveDataTypeable #-}

module Graph.MST.Plain where

import qualified Graph.Weighted as W
import qualified Graph.MST.Kruskal
import Graph.Kreisfrei

import Graph.Util


import Autolib.Dot ( peng, Layout_Program (..) )
import Autolib.Graph.Util ( isZusammen , anzKanten , anzKnoten )
import Autolib.Graph.SpanTree ( weight )
import qualified Autolib.Reporter.Set ( eq , subeq )
import Autolib.Hash ( Hash , hash )
import Autolib.FiniteMap ( FiniteMap , listToFM )

import Inter.Types ( Make , direct )
import Data.Typeable ( Typeable )
import qualified Challenger as C

import Data.Maybe ( fromMaybe )
import qualified Data.Map as M

-------------------------------------------------------------------------------

data MST = MST deriving ( Eq, Ord, Show, Read, Typeable )

instance C.Partial MST ( W.Graph Int Int ) (Int, Graph Int)  where

    report MST wg = do

        inform $ vcat 
	 [ text "Gesucht ist ein Minimalgerüst des gewichteten Graphen"
	 , nest 4 $ toDoc wg
	 ]
        let ( g, w ) = W.extract wg
        -- FIXME: at ToDot instance for weighted graphs
        peng $   g { layout_program = Dot
		   , layout_hints = [ "-Nshape=ellipse" , "-Gsize=\"5,5\"" ]
		   } 

        inform $ text "Sie sollen auch das Gewicht des Gerüstes angeben."

    initial MST wg = 
        let ( g, w ) = W.extract wg
            n  = anzKanten g
            ks = head $ teilmengen (div n 2) (kanten g)
            g0  = mkGraph (knoten g) ks
        in ( 100 , g0 )

    partial MST wg (wt, t) = do
        let ( g, w ) = W.extract wg

        let kn_g = ( text "Knotenmenge V(G) des Graphen" , knoten g )
	    kn_t = ( text "Knotenmenge V(T) in Ihrer Lösung" , knoten t )

	Autolib.Reporter.Set.eq kn_g kn_t

        let ka_g = ( text "Kantenmenge E(G) des Graphen" , kanten g )
	    ka_t = ( text "Kantenmenge E(T) in Ihrer Lösung" , kanten t )

	Autolib.Reporter.Set.subeq ka_t ka_g

        inform $ vcat [ text "Stimmen das von Ihnen berechnete Gesamtgewicht"
		      , nest 4 $ toDoc wt
		      , text "und das Gesamtgewicht Ihrer Einsendung überein?"
		      ]

        let real_wt = sum $ do
                k <- lkanten t
                return $ fromMaybe ( error "kein Gewicht" ) 
                       $ M.lookup k w

        when ( real_wt /= wt ) $ reject $ text "Nein."
        inform $ text "Ja."
    
    total MST wg (wt,t) = do
        inform $ text "Ihr Graph hat die Gestalt:"

	peng $ t { layout_program = Dot
		   , layout_hints = [ "-Nshape=ellipse" , "-Gsize=\"5,5\"" ]
		   }

        inform $ text "Ist dieser Graph kreisfrei?"
        case kreisfrei t of
            Nothing -> inform $ text "ja"
            Just k  -> reject $ text "nein, diese Kante liegt auf einem Kreis:" </> toDoc k

        inform $ text "Ist das Gewicht minimal?"
        let wmin = Graph.MST.Kruskal.weight $ W.extract wg
        when ( wt > wmin ) $ reject 
	      $ text "Nein. Es gibt ein Gerüst geringeren Gewichts!"
        when ( wt < wmin ) $ reject
              $ error "Das kann eigentlich nicht sein."
        inform $ text "Ja."

instance C.Measure MST (W.Graph Int Int) (Int,Graph Int) where
    measure _ _ (wt,t) = fromIntegral $ wt

-------------------------------------------------------------------------------

make :: Make
make = direct MST $ W.example 10
