-- -*- mode: haskell -*-



import FiniteMap
import Challenger

import SAT



student = Aufgabe { problem = SAT

                  , instanz = f

		  , beweis  =b
                  }



--Beispielstrukturen 


--Erf�llbare Aussage mit Belegung b
f :: Formel 
f = [(Pos "x", Pos "y", Pos "z")
    ,(Neg "x", Neg "y", Neg "z")
    ]

b :: Belegung
b = listToFM [("x", True),("y", False),("z", False)]

-- Gegenbeispiel:nicht erf�llbare 3SAt

fgegen :: Formel 
fgegen = [(Pos "x", Pos "y", Pos "z")
	 ,(Neg "x", Neg "y", Neg "z")
	 ,(Neg "x", Neg "y", Pos "z")
	 ,(Neg "x", Pos "y", Pos "z")
	 ,(Neg "x", Pos "y", Neg "z")
	 ,(Pos "x", Neg "y", Neg "z")
	 ,(Pos "x", Neg "y", Pos "z")
	 ,(Pos "x", Pos "y", Neg "z")
	 ]

--F�r alle Belegungen ist nicht erf�llbar. Beispiel:
bgegen :: Belegung
bgegen = listToFM [("x", True),("y", False),("z", True)]
