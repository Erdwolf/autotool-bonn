{-# OPTIONS -fallow-incoherent-instances #-}

module Graph.Bisekt.Plain where

--  $Id$

import Graph.Bisekt.Data

import Graph.Util

import Autolib.Dot ( peng, Layout_Program (..) )
import Autolib.Graph.Adj ( adjazenz_matrix , schoen )
import Autolib.Graph.Ops ( unlinks )
import Autolib.Xml
-- import Text.XML.HaXml.Haskell2Xml
import Autolib.Reader
import Autolib.Set

import Inter.Types
import Autolib.Set
import qualified Challenger as C
import qualified Autolib.Reporter.Set
import Data.Typeable

data Bisect = Bisect deriving ( Eq, Ord, Show, Read, Typeable )

instance ( GraphC a, Show a) -- , Haskell2Xml (Solution a) )
    => C.Partial Bisect (Int, Graph a) (Solution a) where

    report _ (_,g) = do
        inform $ vcat $ map text
         [ "Gegeben ist die Adjazenzmatrix"
	 , schoen $ adjazenz_matrix g
	 , "des Graphen G."
	 , "Gesucht ist eine Kantenmenge M von Kanten von G, die G bisektiert."
	 , ""
	 , "Eine Kantenmenge M bisektiert G, wenn durch Entfernen von M der Graph G in zwei Teilgraphen zerfällt,"
	 , "die sich in ihrer Knotenzahl um höchstens 1 unterscheiden und zwischen denen keine Kanten verlaufen."
	 , ""
	 , "Sie sollen die Kantenmenge M und die Knotenmengen der beiden Teilgraphen angeben."
	 ]

    initial _ (w,g) = 
	let ks = head $ drop w $ teilmengen (pred w) (kanten g)
            n = cardinality (knoten g)
	    ns = head $ drop (div n 2) $ teilmengen (div n 2) (knoten g)
        in Solution { schnittkanten = ks 
		    , knoten1 = ns 
		    , knoten2 = knoten g `minusSet` ns 
		    }

    partial _ (_,g) (Solution { schnittkanten =ks, knoten1=l,knoten2=r} ) = do

        inform $ text "Unterscheiden sich die Knotenanzahlen höchstens um 1?"

        when ( abs ((cardinality l) - (cardinality r)) > 1 ) 
		 $ reject $ text "Nein."

        inform $ text "Ja."

        let s1 = ( text "Kantenmenge E(G) des Graphen" , kanten g )
	    s2 = ( text "Kantenmenge M in Ihrer Lösung" , ks )

	Autolib.Reporter.Set.subeq s2 s1

        let sl = ( text "Erste Knotenmenge in Ihrer Lösung" , l )
	    sr = ( text "Zweite Knotenmenge in Ihrer Lösung" , r )
            sk = ( text "Knotenmenge des Graphen" , knoten g )

	Autolib.Reporter.Set.partition [sl,sr] sk
    
    total _ (_,g) (Solution { schnittkanten =ks, knoten1=l,knoten2=r})  = do

        let u = unlinks g $ setToList ks

        inform $ text "Der Graph, der durch Entfernen der Kanten in Ihrer Lösung entsteht, hat die Gestalt:"

	peng $ u { layout_program = Circo
		 , layout_hints = [ "-Nshape=ellipse" , "-Gsize=\"5,5\"" ]
		 }

        inform $ vcat 
	       [ text "Verlaufen keine Kanten zwischen den Knotenmengen" 
	       , nest 4 $ toDoc l
	       , text "und"
	       , nest 4 $ toDoc r
	       , text "?"
	       ]

        let no = do nl <- setToList l
		    nr <- setToList r
		    guard $ or [ kante nl nr `elementOf` kanten u
			       , kante nr nl `elementOf` kanten u
			       ]
		    return (nl,nr)

        when ( not $ null no ) $ reject $ vcat
		 [ text "Nein, diese Knoten sind durch eine Kante miteinander verbunden:"
		 , nest 4 $ toDoc $ mkSet no
		 ]

        inform $ text "Ja."


instance ( GraphC a, Show a )
    => C.Measure Bisect (Int, Graph a) (Solution a) where
    measure _ _ s = fromIntegral $ cardinality $ schnittkanten s

{-
instance ( Ord a , ToDoc a, ToDoc [a] , Reader a , Reader [a] ) 
    => Container (Set (Kante a),Set a,Set a) String where
    label _ = "(Kantenmenge,Knotenmenge,Knotenmenge)"
    pack = show
    unpack = read
-}

-------------------------------------------------------------------------------

make :: Make
make = direct Bisect ( 5 :: Int, mkGraph (mkSet ns) (mkSet es) )
    where ns = [ 1..6 ] :: [Int]
	  es = map (uncurry kante) 
	       [      (1,2)      ,(1,4),(1,5)
	       ,(2,1)      ,(2,3)      ,(2,5),(2,6)
	             ,(3,2)      ,(3,4),(3,5),(3,6)
	       ,(4,1)      ,(4,3)      ,(4,5)
	       ,(5,1),(5,2),(5,3),(5,4)      ,(5,6)
	             ,(6,2),(6,3)      ,(6,5)
	       ]:: [ Kante Int ]
