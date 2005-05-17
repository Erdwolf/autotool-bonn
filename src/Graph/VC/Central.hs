-- $Id$

module Graph.VC.Central 

( make_fixed
, make_quiz
, VC ( VC )
)

where

import Graph.VC.SAT ( vc )
import qualified Graph.VC.Param as P
import SAT.Types

import Graph.Util
import Autolib.Graph.Ops ( normalize )
import Autolib.Dot ( peng, Layout_Program (..) )
import qualified Autolib.Reporter.Set ( subeq )
import Autolib.Util.Zufall ( eins , genau )

import Inter.Quiz ( Generator , generator , Project , project , quiz )
import Inter.Types ( Make , direct )
import Data.Typeable ( Typeable )

import qualified Challenger as C

import Data.List ( (\\) )

-------------------------------------------------------------------------------

data VC = VC deriving ( Eq , Ord , Show , Read , Typeable )

instance C.Partial VC (Graph Int,Int) (Set Int) where

    report VC (g,c) = do

        inform $ vcat
	       [ text "Geben Sie eine Knoten�berdeckung mit h�chstens"
	       , nest 4 $ toDoc c
	       , text "Knoten des Graphen"
	       , nest 4 $ toDoc g
	       ]
        peng $ g { layout_program = Dot
		 , layout_hints = [ "-Nshape=ellipse" ]
		 } 
        inform $ text "an!"

    initial VC (g,c) = head $ teilmengen (pred c) (knoten g)

    partial VC (g,c) ns = do

        inform $ text $ foldl1 (++) 
		      [ "Ist die Knotenanzahl h�chstens "
		      , show c
		      , "?"
		      ]

        when ( cardinality ns > c ) $ reject $ nest 4 $ text "Nein."

        inform $ nest 4 $ text "Ja."

        Autolib.Reporter.Set.subeq ( text "Knoten in ihrer Einsendung" , ns )
	                           ( text "Knoten des Graphen" , knoten g )

    total VC (g,_) ns = do

        inform $ text "Sind alle Kanten abgedeckt?"

        let no = do k@(Kante { von = v , nach = n }) <- lkanten g
		    guard $ not $ or [ elementOf v ns
				     , elementOf n ns
				     ]
		    return k

        when ( not $ null no ) $ reject $ vcat
		 [ text "Nein. Mindestens diese Kanten sind nicht abgedeckt:"
		 , nest 4 $ toDoc $ take 2 no
		 ]

        inform $ text "Ja."

instance C.Measure VC (Graph Int,Int) (Set Int) where
    measure VC _ ns = fromIntegral $ cardinality ns

make_fixed :: Make
make_fixed = let (g,c) = vc  $ And [ Or [ Pos "x" , Pos "y" , Pos "z" ] 
				   , Or [ Neg "x" , Pos "y" , Neg "z" ]
				   ]
             in direct VC ( normalize g , c )

instance Generator VC P.Param ( Graph Int , Int , Set Int ) where
    generator VC conf _ = do

        let ns = [ 1 .. P.knoten conf ]

        ls <- genau (P.deck_knoten_moeglich conf) ns

        let rs = ns \\ ls

        ks <- sequence $ replicate (P.kanten conf) $ do
	      x <- eins ls
	      y <- eins rs
	      return $ kante x y

        ks0 <- sequence $ replicate (P.kanten_in_deckmenge conf) $ do
	       x <- eins ls
	       y <- eins ls
	       return $ kante x y

        return ( mkGraph (mkSet ns) (mkSet $ ks ++ ks0)
	       , P.deck_knoten_maximum conf
	       , mkSet ls
	       )

instance Project VC (Graph Int,Int,Set Int) (Graph Int,Int) where
    project VC (g,c,_) = (g,c)

make_quiz :: Make
make_quiz = quiz VC P.p0
