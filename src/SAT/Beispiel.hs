module SAT.Beispiel where

-- -- $Id$

import SAT.Types
import Autolib.FiniteMap

l1 = Pos (read "x") :: Literal

v1 = read "x" :: Variable

k1 = Or [Pos $ read "x", Neg $ read  "y", Pos $ read  "z"] :: Klausel
k2 = Or [Neg  $ read "x", Pos  $ read "y", Pos  $ read "z"] :: Klausel

formel = And [ k1, k2 ] :: Formel

b1 :: Belegung
b1 = listToFM [ ( read "x", True), ( read "y", False) ]

belegung :: Belegung
belegung = listToFM 
	 [( read "x", False), ( read "y" , True), ( read "z" , False)]

