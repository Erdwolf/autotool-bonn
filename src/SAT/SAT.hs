
-- | Korrekturfunktion f�r 3SAT
-- 
-- Autor: Mohammad Esad-Djou
-- bss98aou@studserv.uni-leipzig.de

-- -- $Id$

-- Gegeben: Aussagenlogik Formel F in konjunktive Normalform 
--         (mit genau 3 Konjunktionsgliedern)
-- Gesucht: Ist F erf�llbar?
-- Problem ist 3SAT, d.h. Erf�llbarkeitsproblem. 
--     => ein Datentyp mit einem konstanten Konstruktor
-- Instanz ist Formel F. 
--     => ein Datenstruktur
-- Beweis ist Belegung b, die Formel f erf�llt.
--     => ein Datenstruktur


module SAT.SAT 

( SAT (..)
, Variable, Literal (..)
, Klausel, Formel, Belegung
, module Data.FiniteMap

)

where

import Data.FiniteMap
import ReadFM

import Challenger.Partial

import ToDoc
import Control.Monad (guard)
import Sets
import System

-- import Step
-- import Interactive.Type
-- import qualified Component as C
import Reporter
import Maybe

-- ???
import Number
import Iso

import SAT.Types
import SAT.Wert
-- import SAT.Inter -- ersetzt durch Quiz


---------------------------------------------------------------------------

instance Partial SAT Formel Belegung where

    describe SAT f = vcat
        [ text "finden Sie eine erf�llende Belegung f�r die Formel"
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
	     , text "geh�ren gar nicht zur Formel:"
	     , nest 4 $ toDoc out
	     ]

	let wrong = do 
	        klaus <- klauseln f
		False <- maybeToList $ wert klaus b
		return klaus
	when ( not $ null wrong ) $ reject $ vcat
	     [ text "Diese vollst�ndig belegten Klauseln sind nicht erf�llt:"
	     , nest 4 $ toDoc wrong
	     ]
	inform $ text "Alle vollst�ndig belegten Klauseln sind erf�llt."

        let open = do
		 klaus <- klauseln f
		 Nothing <- return $ wert klaus b
		 return klaus
	inform $ vcat
	       [ text "Diese Klauseln noch nicht erf�llt:"
	       , nest 4 $ toDoc open
	       ]

    total SAT f b = do
        let fehl = minusSet ( variablen f ) ( mkSet $ keysFM b )
	when ( not $ isEmptySet fehl ) $ reject $ vcat
	     [ text "Diese Variablen sind nicht belegt:"
	     , nest 4 $ toDoc fehl
	     ]
	inform $ text "Alle Variablen sind belegt."





