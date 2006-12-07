-- | 3SAT
-- 
-- Mohammad Esad-Djou (c) 2002
-- Überarbeitung: Johannes Waldmann (c) 2004, ..

--  $Id$


module SAT.SAT 

( SAT (..)
, Variable, Literal (..)
, Klausel, Formel, Belegung
, module Autolib.FiniteMap

, make_fixed
, make_quiz

)

where

import Autolib.FiniteMap

import Challenger.Partial
import Inter.Types
import Inter.Quiz

import Autolib.ToDoc
import Control.Monad (guard)
import Autolib.Set

-- import System

-- import Step
-- import Interactive.Type
-- import qualified Component as C
import Autolib.Reporter
import Data.Maybe

import SAT.Types
import SAT.Wert
import SAT.Generator
import SAT.Param
import SAT.Beispiel

---------------------------------------------------------------------------

instance Partial SAT Formel Belegung where

    describe SAT f = vcat
        [ text "finden Sie eine erfuellende Belegung fuer die Formel"
	, nest 4 $ toDoc f
	]

    initial SAT f = 
        let v : vs = setToList $ variablen f
	in  listToFM [ ( v, True ) ]

    partial SAT f b = do

        let domain = mkSet $ keysFM b
	    out = minusSet domain ( variablen f )
        when ( not $ isEmptySet out ) $ reject $ vcat
	     [ text "Diese Variablen der Belegung"
	     , text "gehoeren gar nicht zur Formel:"
	     , nest 4 $ toDoc out
	     ]

	let wrong = do 
	        klaus <- klauseln f
		False <- maybeToList $ wert klaus b
		return klaus
	when ( not $ null wrong ) $ reject $ vcat
	     [ text "Diese vollstaendig belegten Klauseln sind nicht erfuellt:"
	     , nest 4 $ toDoc wrong
	     ]
	inform $ text "Alle vollstaendig belegten Klauseln sind erfuellt."

        let open = do
		 klaus <- klauseln f
		 Nothing <- return $ wert klaus b
		 return klaus
	inform $ vcat
	       [ text "Diese Klauseln noch nicht erfuellt:"
	       , nest 4 $ toDoc open
	       ]

    total SAT f b = do
        let fehl = minusSet ( variablen f ) ( mkSet $ keysFM b )
	when ( not $ isEmptySet fehl ) $ reject $ vcat
	     [ text "Diese Variablen sind nicht belegt:"
	     , nest 4 $ toDoc fehl
	     ]
	inform $ text "Alle Variablen sind belegt."


make_fixed :: Make
make_fixed = direct SAT SAT.Beispiel.formel

instance Generator SAT Param ( Formel, Belegung ) where
    generator SAT conf key = hgen2 conf

instance Project SAT ( Formel, Belegung ) Formel where
    project SAT ( f, b ) = f

make_quiz :: Make
make_quiz = quiz SAT $ SAT.Param.p 5





