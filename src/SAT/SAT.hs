-- *****************************************************************
-- Korrekturfunktion für 3SAT
-- 
-- Autor: Mohammad Esad-Djou
-- bss98aou@studserv.uni-leipzig.de
-- *****************************************************************
-- ************************  3SATProblem:  *************************
--Gegeben: Aussagenlogik Formel F in konjunktive Normalform 
--         (mit genau 3 Konjunktionsgliedern)
--Gesucht: Ist F erfüllbar?
--Problem ist 3SAT, d.h. Erfüllbarkeitsproblem. 
--     => ein Datentyp mit einem konstanten Konstruktor
--Instanz ist Formel F. 
--     => ein Datenstruktur
--Beweis ist Belegung b, die Formel f erfüllt.
--     => ein Datenstruktur
-- *****************************************************************
module SAT.SAT where

import FiniteMap
import ReadFM
import Challenger
import ToDoc
import Monad (guard)
import Set
import Auswertung
import Report
import Right

-- *****************************************************************
--Bemerkungen: 
--1. Aussagenlogische Formel in konjunktiver Normalform (KNF):
--Literal = Variable oder nonVariableVariable
--Klausel= Literal || Literal ||...|| Literal 	
--KNF = Klausel && Klausel && ... && Klausel
-- *****************************************************************

data SAT = SAT deriving Show

-- Elementare Def.
data Literal = Pos Variable | Neg Variable
    deriving (Show,Read)

instance ToDoc Literal where
   toDoc l = text (show l)

--Klausel = Tripeln von Literalen
type Klausel = (Literal,Literal,Literal)

--Formeln in 3KNF = Liste von Klauseln
type Formel = [Klausel]

--Variable = String
type Variable = String 

--Belegung = Variable -> B00l
type Belegung = FiniteMap Variable Bool

instance Problem SAT Formel Belegung where 

   verifiziere SAT f b = 
    let w = wert_formel f b
	ks = [ k | k <- f , wert_klausel k b == False ]
    in  if w then (w, text "Die Belegung erfüllt die Formel.")
             else (w, text "Diese Klauseln sind nicht erfüllt:" <+> toDoc ks ) 


------------------------------------------------
l1 = Pos "x" :: Literal
v1 = "x" :: Variable
k1 = (Pos "x", Neg "y", Pos "z") :: Klausel
f1 = [ k1 ] :: Formel

b1 :: Belegung
b1 = listToFM [ ("x", True), ("y", False) ]
b2 :: Belegung
b2 = listToFM [("x", False), ("y" , True), ("z" , False)]

wahrheitswert :: Formel -> Belegung -> Bool
-- falsch:
wahrheitswert f b = True

wert_formel :: Formel -> Belegung -> Bool
wert_formel f b = 
    and [ wert_klausel k b | k <- f ]


wert_klausel 	:: Klausel -> Belegung -> Bool
wert_klausel (l1,l2,l3) b = wert_literal l1 b || wert_literal l2 b || wert_literal l3 b

wert_literal 	:: Literal -> Belegung -> Bool
wert_literal  (Pos v) b = wert_variable v b
wert_literal  (Neg v) b = not (wert_variable v b)
			
wert_variable 	:: Variable -> Belegung -> Bool
wert_variable v b =
    case lookupFM b v of
	Nothing -> error "variable nicht in belegung."
	Just w  -> w


-- Beispiel
student = Aufgabe { problem = SAT

                  , instanz = f

		  , beweis  = b'
                  }



--Beispielstrukturen 


--Erfüllbare Aussage mit Belegung b
f :: Formel 
f = [(Pos "x", Pos "y", Pos "z")
    ,(Neg "x", Neg "y", Neg "z")
    ]

b' :: Belegung
b' = listToFM [("x", True),("y", False),("z", False)]

-- Gegenbeispiel:nicht erfüllbare 3SAt

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

--Für alle Belegungen ist nicht erfüllbar. Beispiel:
bgegen :: Belegung
bgegen = listToFM [("x", True),("y", False),("z", True)]
