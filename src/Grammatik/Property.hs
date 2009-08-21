{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, OverlappingInstances, TemplateHaskell #-}

module Grammatik.Property where

import Grammatik.Type
import Grammatik.CF.Eindeutig
import Grammatik.Hierarchie

import Condition

import Autolib.Reporter.Type
import qualified Autolib.Reporter.Checker as C

import Autolib.Reader
import Autolib.ToDoc

import Data.Typeable

data Property = Eindeutig Int
	      | Typ Int
	      | Monoton
	      | Kontextsensitiv
	      | Kontextfrei
	      | Linear
	      | Rechtslinear
	      | Linkslinear
              | At_Most_One_Terminal_In_Rhs
	      | Epsfrei
	      | Kettenfrei
	      | Chomsky_Normal
	      | Greibach_Normal
     deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc] [''Property])

instance Condition  Property Grammatik where
    condition p a = C.run ( Grammatik.Property.check p ) a

instance Explain Property where
    explain p = C.condition ( Grammatik.Property.check p )

check :: Property -> C.Type   Grammatik
check p = case p of
    Typ 0 -> typ0
    Typ 1 -> typ1
    Typ 2 -> typ2
    Typ 3 -> typ3
    Monoton -> monoton
    Kontextsensitiv ->  kontextsensitiv
    Kontextfrei -> kontextfrei
    Linear ->  linear
    Rechtslinear -> rechtslinear
    Linkslinear ->  linkslinear
    Epsfrei ->  epsfrei
    Kettenfrei ->  kettenfrei
    Chomsky_Normal -> chomsky
    Greibach_Normal ->  greibach
    Eindeutig i -> eindeutig i
    At_Most_One_Terminal_In_Rhs -> at_most_one_terminal_in_rhs 

-- local variables:
-- mode: haskell
-- end:
