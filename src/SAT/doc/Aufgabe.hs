-- -*- mode: haskell -*-
-- -- $Id$

import Challenger
import SAT.SAT

student = Aufgabe 
	{ problem = SAT
        , instanz =  [ (Pos "x", Pos "y", Pos "z")
		     , (Neg "x", Pos "x", Pos "y")
		     ]
	, beweis  = listToFM [("x", True),("y", False),("z", False)]
        }

