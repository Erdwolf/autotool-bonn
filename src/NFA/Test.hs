module NFA.Test where

--  $Id$

import NFA.Property

import Autolib.NFA
import Autolib.NFA.Trim
import Autolib.NFA.Check

import Autolib.Reporter
import Autolib.Reporter.Set
import Autolib.ToDoc
import Autolib.Size

test :: NFAC c s
     => Property c 
     -> NFA c s
     -> Reporter ()
test (Min_Size s) aut = do
    assert ( size aut >= s ) 
	   $ text "Zustandszahl ist wenigstens" <+> toDoc s <+> text "?"
test (Max_Size s) aut = do
    assert ( size aut <= s ) 
	   $ text "Zustandszahl ist h�chstens" <+> toDoc s <+> text "?"
test (Alphabet m) aut = do
    subeq ( text "Alphabet des Automaten", alphabet aut )
          ( toDoc m, m )
test (Deterministic) aut = do
    deterministisch aut
test (Reduced) aut = do
    inform $ text "Der Automat soll reduziert sein."
    subeq ( text "alle Zust�nde", states aut )
	  ( text "erreichbare Zust�nde", states $ reachable aut )
    subeq ( text "alle Zust�nde", states aut )
	  ( text "produktive Zust�nde",  states $ productive aut )
    
{-
	      | Minimal
	      | Complete
-}

test prop aut = do
    reject $ fsep [ text "test f�r", toDoc prop
		  , text "noch nicht implementiert"
		  ]
