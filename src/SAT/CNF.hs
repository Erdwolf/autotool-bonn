{-# OPTIONS -fglasgow-exts #-}

-- | Bemerkungen: 
-- 1. Aussagenlogische Formel in konjunktiver Normalform (KNF):
-- Literal = Variable oder nonVariableVariable
-- Klausel= Literal || Literal ||...|| Literal 	
-- KNF = Klausel && Klausel && ... && Klausel

module SAT.CNF where

import Autolib.TES.Identifier

import Autolib.FiniteMap
import Autolib.Set
import Autolib.ToDoc as T
import Autolib.Reader as R

import Data.Typeable

data CNF_SAT = CNF_SAT deriving ( Show, Read, Typeable )

-- | Elementare Def.

type Variable = Identifier
data Literal = Pos Variable | Neg Variable
    deriving ( Eq, Ord, Typeable)

unLiteral ( Pos v ) = v
unLiteral ( Neg v ) = v

opposite ( Pos v ) = Neg v
opposite ( Neg v ) = Pos v

instance ToDoc Literal where
    toDoc ( Pos v ) = text " " <+> toDoc v
    toDoc ( Neg v ) = text "!" <+> toDoc v

instance Reader Literal where
    reader = do
        signed <- option Pos $ do my_symbol "!" ; return Neg
	this <- reader
	return $ signed this

-- | Klausel = Tripeln von Literalen

data Klausel = Or [ Literal ] deriving ( Eq, Ord , Typeable)

literale :: Klausel -> [ Literal ]
literale (Or lits) = lits

instance ToDoc Klausel where
    toDoc ( Or lits ) = parens $ T.sepBy ( text "||" ) $ map toDoc lits

instance Reader Klausel where
    reader = my_parens $ do
        lits <- reader `R.sepBy` my_symbol "||"
	return $ Or lits

-- | Formeln in 3KNF = Liste von Klauseln
data Formel = And [Klausel] deriving ( Typeable )

klauseln :: Formel -> [ Klausel ]
klauseln (And cls) = cls

instance ToDoc Formel where
    toDoc ( And klaus ) = vcat $ do
        ( i, k ) <- zip [ 0.. ] klaus
	let prefix = text $ if i == 0 then "  " else "&&"
	return $ prefix <+> toDoc k

instance Reader Formel where
    reader = do
        klaus <- reader `R.sepBy` my_symbol "&&"
	return $ And klaus



type Belegung = FiniteMap Variable Bool

variablen :: Formel -> Set Variable
variablen (And cls) = mkSet $ do
    Or lits <- cls
    lit <- lits
    return $ case lit of Pos v -> v ; Neg v -> v


