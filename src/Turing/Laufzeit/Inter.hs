module Turing.Laufzeit.Inter where

-- $Id$

import Turing.Type
import Turing.Laufzeit.Type

import Inter.Types
import Reporter
import ToDoc
import Informed

step :: String		-- name der variante 
     -> Laufzeit
     -> Var TM Laufzeit
step v l =
    Var { problem = TM
	, variant = "STEP" ++ v
	, key = \ matrikel -> do
	      return matrikel
	, gen = \ matrikel -> do
	      inform $ text "Konstruieren Sie eine Turingmaschine"
	      inform $ text "mit Laufzeitfunktion" <+> info l
	      return l
	}
