obsolete module SAT.Inter where

-- -- $Id$

import SAT.Types
import SAT.Beispiel

import Inter.Types
import Reporter
import ToDoc

var = Var    {  problem = SAT
	     , variant = "simple"
	     , key = \ matrikel -> do
	         -- d. h. jeder bekommt immer die gleiche aufgabe
	         return matrikel
	     , gen = \ matrikel -> do
                 let f = bsp_formel
	         inform $ text "Finden Sie eine erf�llende Belegung f�r"
	         inform $ toDoc f
	         return f
	     }

