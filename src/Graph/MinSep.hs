module Graph.MinSep where

--  $Id$

import Graph.Util
import Graph.Iso

import Autolib.Graph.Basic
import Autolib.Graph.Ops
import Autolib.Graph.Kneser ( petersen )

import Inter.Types
import Autolib.Reporter
import Autolib.Set
import Autolib.Util.Teilfolgen
import qualified Challenger as C

import Autolib.Dot ( peng )

import Data.Typeable
import Data.List ( partition )

data MinSep = MinSep deriving ( Eq, Ord, Show, Read, Typeable )

instance OrderScore MinSep where
    scoringOrder _ = Increasing

instance C.Measure MinSep ( Graph Int ) ( (Int, Int), Set Int ) where
    measure MinSep g ( (a, b), s ) = fromIntegral $ cardinality s

instance C.Partial MinSep ( Graph Int ) ( (Int, Int), Set Int ) where

    report MinSep g = do
        inform $ vcat 
	       [ text "Finden Sie für diesen (nicht chordalen) Graphen" 
	       , nest 4 $ toDoc g 
	       ]
        peng g
	inform $ vcat 
	       [ text "Knoten a, b und einen minimalen (a,b)-Separator,"
	       , text "der keine Clique ist."
	       ]

    initial p g = 
        let x : y : zs = lknoten g
	in  ( (x, y), mkSet $ take 3 zs )

    partial p g ( (a, b), s ) = do
        inform $ vcat [ text "Ihre Eingabe:"
		      , nest 4 $ vcat [ text "(a, b) = " <+> toDoc (a, b)
				      , text "s      = " <+> toDoc s
				      ]
		      ]
	when ( not $ a `elementOf` knoten g )
	     $ reject $ text "a gehört nicht zu V(G)"
	when ( not $ b `elementOf` knoten g )
	     $ reject $ text "b gehört nicht zu V(G)"
	when ( not $ subseteq s (knoten g) ) 
	     $ reject $ text "s ist keine Teilmenge von V(G)"
    
    total p g ( (a, b), s ) = do
	when ( a == b )
	     $ reject $ text "a = b"
	assert ( not $ kante a b `elementOf` kanten g )
	       $ text "a und b sollen nicht benachbart sein"
	assert ( separates g ( (a, b), s ))
	       $ text "a und b sollen durch s getrennt werden"
	let ts = do 
	        x <- setToList s
		let t = s `minusSet` unitSet x
		guard $ separates g ((a, b), s)
		return t
        case ts of
	     [] -> return ()
	     t : _ -> reject $ vcat
	       [ text "s  ist kein minimaler  (a.b)-Separator,"
	       , text "denn diese echte Teilmenge  t  von  s"
	       , text "ist auch ein  (a,b)-Separator:"
	       , nest 4 $ toDoc t
	       ]
	assert ( is_clique g s )
	       $ text "s ist eine Clique?"
	     

make :: Make
make = direct MinSep petersen


separates :: ( ToDoc [a], GraphC a )
	  => Graph a -> ((a, a), Set a) -> Bool
separates g ( (a, b), s ) =
    let h = restrict ( knoten g `minusSet` s ) g
    in  not $ b `elementOf` reachables h a
