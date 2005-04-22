module Graph.MST.Plain where

--  $Id$

import Graph.Util

import Autolib.Dot ( peng, Layout_Program (..) )
import Autolib.Graph.Util ( isZusammen , anzKanten , anzKnoten )
import Autolib.Graph.SpanTree ( weight )
import qualified Autolib.Reporter.Set ( eq , subeq )
import Autolib.FiniteMap ( FiniteMap , listToFM , lookupWithDefaultFM )
import Autolib.Hash ( Hash , hash )

import Inter.Types ( Make , direct )
import Data.Typeable ( Typeable )
import qualified Challenger as C

-------------------------------------------------------------------------------

data MST = MST deriving ( Eq, Ord, Show, Read, Typeable )

type W = FiniteMap (Kante Int) Int

wfun :: W -> Kante Int -> Int
wfun w = lookupWithDefaultFM w (error "Gewichte nicht vollständig!")

-- | to just satisfy peng's constraints
instance Eq (Graph Int,Kante Int -> Int) where (x,_) == (y,_) = x == y
instance Hash (Graph Int,Kante Int -> Int) where hash (g,_) = hash g

instance C.Partial MST (Int,Graph Int,W) (Int,Graph Int)  where

    report _ (_,g,w) = do

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

        let real_wt = weight t (wfun w)

        inform $ vcat [ text "Stimmen das von Ihnen berechnete Gesamtgewicht"
		      , nest 4 $ toDoc wt
		      , text "und das Gesamtgewicht Ihrer Einsendung"
		      , nest 4 $ toDoc real_wt
		      , text "überein?"
		      ]

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

instance C.Measure MST (Int,Graph Int,W) (Int,Graph Int) where
    measure _ _ (_,g) = fromIntegral $ cardinality $ kanten g

-------------------------------------------------------------------------------

make :: Make
make = let g = mkGraph (mkSet [1,2,3,4,5,6]) 
                       (mkSet $ map (uncurry kante)
			[(1,5),(2,5),(2,3),(3,3),(5,6),(4,5),(4,6),(1,6)]
		       ) :: Graph Int
       in direct MST ( 34 :: Int
		     , g
		     , listToFM $ do
		       k@(Kante {von=v,nach=n}) <- setToList $ kanten g
		       return $ (k,v+n)
		     )
