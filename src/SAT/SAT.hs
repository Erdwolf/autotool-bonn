
-- Korrekturfunktion für 3SAT
-- 
-- Autor: Mohammad Esad-Djou
-- bss98aou@studserv.uni-leipzig.de

-- $Id$

-- Gegeben: Aussagenlogik Formel F in konjunktive Normalform 
--         (mit genau 3 Konjunktionsgliedern)
-- Gesucht: Ist F erfüllbar?
-- Problem ist 3SAT, d.h. Erfüllbarkeitsproblem. 
--     => ein Datentyp mit einem konstanten Konstruktor
-- Instanz ist Formel F. 
--     => ein Datenstruktur
-- Beweis ist Belegung b, die Formel f erfüllt.
--     => ein Datenstruktur


module SAT.SAT 

( SAT (..)
, Variable, Literal (..)
, Klausel, Formel, Belegung
, module FiniteMap

)

where

import FiniteMap
import ReadFM

import Challenger.Partial

import ToDoc
import Monad (guard)
import Sets
import System

import Step
import Interactive.Type
import qualified Component as C
import Reporter
import Maybe

-- ???
import Number
import Iso

import SAT.Types
import SAT.Inter

variablen :: Formel -> Set Variable
variablen f = mkSet $ do
    ( l1, l2, l3 ) <- f
    l <- [ l1, l2, l3 ]
    return $ case l of Pos v -> v ; Neg v -> v


---------------------------------------------------------------------------

instance Partial SAT Formel Belegung where

    initial SAT f = 
        let v : vs = setToList $ variablen f
	in  listToFM [ ( v, True ) ]

    partial SAT f b = do

        let domain = mkSet $ keysFM b
	    out = minusSet domain ( variablen f )
        when ( not $ isEmptySet out ) $ reject $ vcat
	     [ text "Diese Variablen der Belegung"
	     , text "gehören gar nicht zur Formel:"
	     , nest 4 $ toDoc out
	     ]

	let wrong = do 
	        klaus <- f
		False <- maybeToList $ m_wert_klausel klaus b
		return klaus
	when ( not $ null wrong ) $ reject $ vcat
	     [ text "Diese vollständig belegten Klauseln sind nicht erfüllt:"
	     , nest 4 $ toDoc wrong
	     ]
	inform $ text "Alle vollständig belegten Klauseln sind erfüllt."
	       

    total SAT f b = do
        let fehl = minusSet ( variablen f ) ( mkSet $ keysFM b )
	when ( not $ isEmptySet fehl ) $ reject $ vcat
	     [ text "Diese Variablen sind nicht belegt:"
	     , nest 4 $ toDoc fehl
	     ]
	inform $ text "Alle Variablen sind belegt."





