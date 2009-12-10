module Graph.MST.Plain where

--  $Id$

import Graph.Util

import Graph.MST.Weight ( Weight (..) , wfun )

import Autolib.Dot ( peng, Layout_Program (..) )
import Autolib.Graph.Util ( isZusammen , anzKanten , anzKnoten )
import Autolib.Graph.SpanTree ( weight )
import qualified Autolib.Reporter.Set ( eq , subeq )
import Autolib.Hash ( Hash , hash )
import Autolib.FiniteMap ( FiniteMap , listToFM )

import Inter.Types ( Make , direct )
import Data.Typeable ( Typeable )
import qualified Challenger as C

-------------------------------------------------------------------------------

data MST = MST deriving ( Eq, Ord, Show, Read, Typeable )

-- | to just satisfy peng's constraints

finite :: (Graph Int,Kante Int -> Int) -> (Graph Int,FiniteMap (Kante Int) Int)
finite (g,w) = (g,listToFM $ do k <- setToList (kanten g); return (k,w k))

instance Eq (Graph Int,Kante Int -> Int) where x == y = finite x == finite y
instance Hash (Graph Int,Kante Int -> Int) where hash = hash . finite

instance C.Partial MST (Int,Graph Int,Weight) (Int,Graph Int)  where

    report _ (_,g,w) = do

        when ( case w of Random _ -> True ; _ -> False ) $ inform $ vcat
	  [ text "ACHTUNG!! Random hat bei direkten Aufgaben keinen Sinn!!"
	  ]

        inform $ vcat 
	 [ text "Gesucht ist ein minimaler spannender Baum des Graphen"
	 , nest 4 $ toDoc g
	 , text "mit den Kantengewichten"
	 , nest 4 $ toDoc w
	 ]

        peng $ ( g { layout_program = Dot
		   , layout_hints = [ "-Nshape=ellipse" , "-Gsize=\"5,5\"" ]
		   } 
	       , wfun w
	       )

        inform $ text "Sie sollen auch das Gesamtgewicht des spannenden Baums angeben."

    initial _ (_,g,w) = 
        let n  = anzKanten g
            ks = head $ teilmengen (div n 2) (kanten g)
            g0  = mkGraph (knoten g) ks
            w0  = weight g0 (wfun w)
        in ( w0 , g0 )

    partial _ (_,g,w) (wt,t) = do

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

        let real_wt = weight t (wfun w)

        when ( real_wt /= wt ) $ reject $ text "Nein."

        inform $ text "Ja."
    
    total _ (wmin,_,w) (wt,t) = do

        inform $ text "Ihr Graph hat die Gestalt:"

	peng $ ( t { layout_program = Dot
		   , layout_hints = [ "-Nshape=ellipse" , "-Gsize=\"5,5\"" ]
		   }
	       , wfun w
	       )

        inform $ text "Handelt es sich um einen Baum?"

        inform $ nest 4 $ text "Gilt |V|=|E|+1?"

        let cnt_kn = anzKnoten t
	    cnt_ka = anzKanten t

        when ( not $ cnt_ka + 1 == cnt_kn ) $ reject $ nest 4 $ text "Nein."

        inform $ nest 4 $ text "Ja."

        inform $ nest 4 $ text "Ist der Graph zusammenhängend?"

        when ( not $ isZusammen t ) $ reject $ nest 4 $ text "Nein."

        inform $ nest 4 $ text "Ja."

        inform $ text "Ist das Gewicht minimal?"

        when ( wt /= wmin ) $ reject 
	      $ text "Nein. Es gibt einen spannenden Baum geringeren Gewichts!"

        inform $ text "Ja."

instance C.Measure MST (Int,Graph Int,Weight) (Int,Graph Int) where
    measure _ _ (_,g) = fromIntegral $ cardinality $ kanten g

-------------------------------------------------------------------------------

make :: Make
make = direct MST ( 96 :: Int
		  , gr
		  , Summe
		  )

gr :: Graph Int
gr = mkGraph (mkSet [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ])
             (mkSet [ kante 1 8, kante 1 9, kante 1 10, kante 2 7
		    , kante 2 9, kante 2 10, kante 3 7, kante 3 8, kante 3 9
		    , kante 3 10, kante 4 7, kante 4 8, kante 4 9, kante 4 10
		    , kante 5 7, kante 5 8, kante 5 9, kante 5 10, kante 6 7, kante 6 8
		    , kante 6 9, kante 6 10, kante 7 8, kante 7 9, kante 7 10
		    , kante 8 9, kante 8 10, kante 9 10
		    ]
	     )