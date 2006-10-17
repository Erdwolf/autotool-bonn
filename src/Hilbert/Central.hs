{-# OPTIONS -fglasgow-exts #-}

module Hilbert.Central ( make_fixed ) where

import Hilbert.Param
import Hilbert.Derivation

import Expression.Op
import Boolean.Op

import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reporter

import Inter.Types

import Data.Typeable

data Hilbert = Hilbert deriving ( Show, Read, Typeable )

instance Partial Hilbert Param Derivation where

    describe _ p = vcat
        [ text "Gesucht ist eine Ableitung für die Formel"
	, nest 4 $ toDoc $ target p
	, text "im Hilbert-Kalkül mit den Axiomen"
	, nest 4 $ toDoc $ axioms p
	]


make_fixed :: Make
make_fixed = direct Hilbert Hilbert.Param.example
