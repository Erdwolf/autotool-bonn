module NFA.Test where

--  $Id$

import NFA.Property

import Autolib.NFA
import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Size

test :: NFAC c s
     => Property c 
     -> NFA c s
     -> Reporter ()
test (Min_Size s) aut = do
    assert ( size aut >= s ) 
	   $ text "Zustandszahl ist wenigstens" <+> toDoc s

{-
	      | Max_Size Int
	      | Alphabet ( Set c )
	      | Deterministic
	      | Minimal
	      | Complete
	      | Trim -- no useless states
-}
