-- -*- mode: haskell -*-
-- -- $Id$

import Challenger
import SAT.SAT

student = Loesung 
	{ problem = SAT
        , ident = Ident { aufgabe = 1 }
        , beweis = listToFM [("x", True),("y", False),("z", False)]
        }

