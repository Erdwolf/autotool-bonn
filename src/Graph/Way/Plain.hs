module Graph.Way.Plain where

--  $Id$

import Graph.Util

import Autolib.Dot ( peng, Layout_Program (..) )
import Autolib.Graph.Adj ( schoen , warshall )

import Inter.Types ( Make , direct )
import Data.Typeable ( Typeable )
import Data.Array ( listArray , elems )
import qualified Challenger as C

-------------------------------------------------------------------------------

data Way = Way deriving ( Eq, Ord, Show, Read, Typeable )

instance C.Partial Way (Int,[Integer]) (Graph Int)  where

    report _ (n,vs) = do
        inform $ vcat $ map text
         [ "Gesucht ist ein Graph mit der Wegematrix"
	 , schoen $ listArray ((1,1),(n,n)) vs
	 ]

    initial _ (n,_) = 
	let ns = [1..pred n]
        in mkGraph (mkSet ns) (mkSet $ map (uncurry kante) $ zip ns $ tail ns)

    partial _ (n,_) g = do

        let n' = cardinality $ knoten g

        inform $ text "Stimmt die Knotenanzahl?"

        when ( n /= n' ) $ reject $ text "Nein."

        inform $ text "Ja."
    
    total _ (_,vs) g = do

        let w = warshall g

        inform $ text "Ihr Graph hat die Gestalt:"

	peng $ g { layout_program = Dot
		 , layout_hints = [ "-Nshape=ellipse" , "-Gsize=\"5,5\"" ]
		 }
        inform $ vcat $ map text [ "Die Wegematrix Ihres Graphen ist:"
				 , schoen w
				 ]

        inform $ text "Ist diese Matrix die geforderte?"

        when ( vs /= Data.Array.elems w ) $ reject $ text "Nein."

        inform $ text "Ja."

instance C.Measure Way (Int,[Integer]) (Graph Int) where
    measure _ _ g = fromIntegral $ cardinality $ kanten g

-------------------------------------------------------------------------------

make :: Make
make = direct Way (3::Int,[1,1,0,1,1,0,0,0,1::Integer])
