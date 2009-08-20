{-# LANGUAGE TemplateHaskell #-}
module SAT.Original_Types where

import Autolib.TES.Identifier

import Autolib.FiniteMap
import Autolib.Set
import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable
-- import Text.XML.HaXml.Haskell2Xml

-- | Bemerkungen: 
-- 1. Aussagenlogische Formel in konjunktiver Normalform (KNF):
-- Literal = Variable oder nonVariableVariable
-- Klausel= Literal || Literal ||...|| Literal 	
-- KNF = Klausel && Klausel && ... && Klausel

data SAT = SAT deriving ( Show, Read, Typeable )

-- | Elementare Def.
data Literal = Pos Variable | Neg Variable
    deriving ( Eq, Ord, Typeable)

unLiteral ( Pos v ) = v
unLiteral ( Neg v ) = v

opposite ( Pos v ) = Neg v
opposite ( Neg v ) = Pos v

$(derives [makeReader, makeToDoc] [''Literal])
-- {-! for Literal derive : ToDoc, Reader, Haskell2Xml !-}


-- | Klausel = Tripeln von Literalen

data Klausel = Or [ Literal ] deriving ( Eq, Ord , Typeable)

literale :: Klausel -> [ Literal ]
literale (Or lits) = lits

$(derives [makeReader, makeToDoc] [''Klausel])
-- {-! for Klausel derive : ToDoc, Reader, Haskell2Xml !-}


-- | Formeln in 3KNF = Liste von Klauseln
data Formel = And [Klausel] deriving ( Typeable )

klauseln :: Formel -> [ Klausel ]
klauseln (And cls) = cls

$(derives [makeReader, makeToDoc] [''Formel])
-- {-! for Formel derive : ToDoc, Reader, Haskell2Xml !-}


type Variable = String 


type Belegung = FiniteMap Variable Bool

variablen :: Formel -> Set Variable
variablen (And cls) = mkSet $ do
    Or lits <- cls
    lit <- lits
    return $ case lit of Pos v -> v ; Neg v -> v

-- local variables:
-- mode: haskell
-- end:
