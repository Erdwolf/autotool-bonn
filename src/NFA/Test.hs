module NFA.Test where

--  $Id$

import NFA.Property

import Autolib.NFA
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
	   $ text "Zustandszahl ist höchstens" <+> toDoc s <+> text "?"
test (Alphabet m) aut = do
    subeq ( text "Alphabet des Automaten", alphabet aut )
          ( toDoc m, m )
{-
	      | Minimal
	      | Complete
	      | Trim -- no useless states
-}
test prop aut = do
    inform $ fsep [ text "test für", toDoc prop
		  , text "noch nicht implementiert"
		  ]
