-- -*- mode: haskell -*-



import FiniteMap
import Challenger

import SAT




student = Loesung {
problem = SAT

                  , ident = Ident { aufgabe = 1 }

                  , beweis = b

                  }



b :: Belegung
b = listToFM [("x", True),("y", False),("z", False)]
