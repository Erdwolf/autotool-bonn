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

    describe _ param = vcat
        [ text "Gesucht ist eine Ableitung für die Formel"
	, nest 4 $ toDoc $ target param
	, text "im Hilbert-Kalkül mit den Axiomen"
	, nest 4 $ toDoc $ axioms param
	]

    initial Hilbert param = 
        -- FIXME, sollte von Parametern abhängen
        Hilbert.Derivation.example 
        
    partial Hilbert param d =
        return ()

    total Hilbert param d = do
        v <- derive ( axioms param ) d
	inform $ vcat
	       [ text "Ihre Ableitung ergibt"
	       , nest 4 $ toDoc v 
	       ]
	when ( v /= target param ) $ reject $ vcat
	     [ text "aber gefordert war"
	     , nest 4 $ toDoc $ target param
	     ] 


make_fixed :: Make
make_fixed = direct Hilbert Hilbert.Param.example
