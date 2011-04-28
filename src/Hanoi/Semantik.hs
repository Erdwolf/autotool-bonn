{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Hanoi.Semantik where

--   $Id$

import Hanoi.Type
import Hanoi.Move
import Hanoi.Restriction

import Condition

import Autolib.FiniteMap
import Autolib.Reporter
import Autolib.ToDoc

import qualified Challenger as C
import Inter.Types
import Data.Typeable

data Hanoi = Hanoi deriving ( Eq, Ord, Show, Read, Typeable )

instance OrderScore Hanoi where
    scoringOrder _ = Increasing

instance C.Partial Hanoi HI [ Zug ] where

    describe p i = 
          vcat [ text "Türme von Hanoi. Finden Sie eine Zugfolge von"
		     , nest 4 $ toDoc ( start i )
		     , text "nach"
		     , nest 4 $ toDoc ( ziel i )
	       , explain $ restriction i
	       ]

    initial p i = [ (A, B) ]

    total   p i b = do
        hof <- moves ( restriction i ) ( start i ) b
        inform $ vcat [ text "Sie erreichen diese Situation:"
		      , nest 4 $ toDoc hof
		      ]
	assert ( hof == ziel i ) $ text "Aufgabe gelöst?"
	return ()


make :: Make
make = direct Hanoi $
    let ts = take 3 [ A .. ]
        leer = listToFM $ do t <- ts ; return ( t, [] )
        ss = [ 1 .. 5 ]
    in  HI { start = addToFM leer A ss
	   , ziel  = addToFM leer B ss
           , restriction = Neighbours
	   }



