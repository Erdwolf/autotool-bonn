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
module SAT.SAT 

( SAT (..)
, Variable, Literal (..)
, Klausel, Formel
, module FiniteMap
)

where

import FiniteMap
import ReadFM
import Challenger
import ToDoc
import Monad (guard)
import Set
import System

-- ???
import Number
import Iso

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
    deriving (Show,Read,Eq,Ord)

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
type Map = [(Literal,Literal)]


variablen :: Formel -> Set Variable
variablen f = mkSet $ do
    ( l1, l2, l3 ) <- f
    l <- [ l1, l2, l3 ]
    return $ case l of Pos v -> v ; Neg v -> v


instance Problem SAT Formel Belegung where 

 
   validiere SAT f b = 
       let vf = variablen f
	   vb = mkSet (keysFM b)
           fb = minusSet vf vb
	   bf = minusSet vb vf
       in
	if not ( isEmptySet fb )
	then ( False, text "Diese Variablen sind nicht belegt:" <+> toDoc fb )
	else if not ( isEmptySet bf )
	then ( False, text "Diese Variablen sind nicht in der Formel:" <+> toDoc bf )
	else (True, text "Die Belegung passt zur Formel.")




   verifiziere SAT f b = 
    let w = wert_formel f b
	ks = [ k | k <- f , wert_klausel k b == False ]
    in  if w then (w, text "Die Belegung erfüllt die Formel.")
             else (w, text "Diese Klauseln sind nicht erfüllt:" <+> toDoc ks ) 

-- Erzeugt HTML-File zur Visualisierung
   getInstanz SAT f b dateiName =
	do 
	 writeFile (dateiName ++ ".html")
	            ("<br><table borders><caption>Diese Formael ist zu erfüllen:</caption>" ++ (erzInstanz f) ++ "</table>")
	 return (dateiName ++ ".html", "html",ExitSuccess)


-- Erzeugt HTML-File zur Visualisierung
   getBeweis SAT f b dateiName =
	do
	 writeFile (dateiName ++ ".html")
		    (erzBeweis b)
	 return (dateiName ++ ".html", "html",ExitSuccess)


instance Number Formel Formel where number = id 

-- Version 1
instance Iso Formel where iso f1 f2 = (mkSet f1) == (mkSet f2)

------------------------------------------------
-- erzeugt den Ausgabestring fuer die HTML Ausgabe der Instanz
erzInstanz :: Formel -> String
erzInstanz f = "<tr><td>" ++ show f ++ "</td></tr>"

-- erzeugt den Ausgabestring fuer die HTML Ausgabe des Beweises
erzBeweis :: Belegung -> String
erzBeweis b = "<tr><td>" ++ show b ++ "</td></tr>"


--belegtest = 
-------------------------------------------------
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
