module SAT.Beispiel where

-- -- $Id$

import SAT.Types

import FiniteMap

--belegtest = 
-------------------------------------------------
l1 = Pos "x" :: Literal
v1 = "x" :: Variable
k1 = Or [Pos "x", Neg "y", Pos "z"] :: Klausel
k2 = Or [Neg "x", Pos "y", Pos "z"] :: Klausel
bsp_formel = And [ k1, k2 ] :: Formel

b1 :: Belegung
b1 = listToFM [ ("x", True), ("y", False) ]
b2 :: Belegung
b2 = listToFM [("x", False), ("y" , True), ("z" , False)]

