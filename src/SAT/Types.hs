module SAT.Types where

-- $Id$

import FiniteMap
import ToDoc

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


