{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Graph.Way.Plain where

--  $Id$

import Graph.Util

import Graph.Way.Input ( Input , matrix , solvability , ex 
		       , Solvability ( Solvable , Unsolvable ) 
		       )

import Autolib.Dot ( peng, Layout_Program (..) )
import Autolib.Graph.Adj ( schoen , warshall )

import Inter.Types ( Make , direct , ScoringOrder (..) , OrderScore (..) )
import Data.Typeable ( Typeable )
import Data.Array ( listArray , elems )
import qualified Challenger as C

-------------------------------------------------------------------------------

data Way = Way deriving ( Eq, Ord, Show, Read, Typeable )

instance OrderScore Way where
    scoringOrder _ = Increasing

instance C.Partial Way Input (Maybe (Graph Int))  where

    report _ inp = do
        let n = length $ matrix inp
        inform $ vcat $ map text
         [ "Gesucht ist ein Graph mit der Wegematrix"
	 , schoen $ listArray ((1,1),(n,n)) $ concat $ matrix inp
	 , foldl1 (++) [ "(Eingabe: \"Nothing\""
		       , " falls Sie behaupten, dass es keinen Graphen"
		       , " mit dieser Wegematrix gibt.)"
		       ]
	 ]

    initial _ inp = 
	let n  = length $ matrix inp
            ns = [1..pred n]
        in Just $ mkGraph (mkSet ns) 
	                  (mkSet $ map (uncurry kante) $ zip ns $ tail ns)

    partial _ _ Nothing = do

        inform $ text "Sie behaupten: Es gibt keinen Graphen mit dieser Wegematrix."

    partial _ inp (Just g) = do

        let n  = length $ matrix inp
        let n' = cardinality $ knoten g

        inform $ text "Stimmt die Knotenanzahl?"

        when ( n /= n' ) $ reject $ text "Nein."

        inform $ text "Ja."
    
    total _ inp Nothing = do
        
        when ( solvability inp == Solvable ) 
	     ( reject $ text "Das stimmt nicht." )

        inform $ text "Das stimmt."

    total _ inp (Just g) = do

        let w = warshall g

        inform $ text "Ihr Graph hat die Gestalt:"

	peng $ g { layout_program = Dot
		 , layout_hints = [ "-Nshape=ellipse" , "-Gsize=\"5,5\"" ]
		 }
        inform $ vcat $ map text [ "Die Wegematrix Ihres Graphen ist:"
				 , schoen w
				 ]

        inform $ text "Ist diese Matrix die geforderte?"

        when ( concat (matrix inp) /= Data.Array.elems w ) 
	     ( reject $ text "Nein." )

        inform $ text "Ja."

instance C.Measure Way Input (Maybe (Graph Int)) where
    measure _ _ (Just g) = fromIntegral $ cardinality $ kanten g
    measure _ _ _        = 0

-------------------------------------------------------------------------------

make :: Make
make = direct Way ex

