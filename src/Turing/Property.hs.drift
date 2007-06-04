-- -*- mode: haskell -*-

module Turing.Property where

import Turing.Type
import Turing.Check

import Condition

import Autolib.Reporter
import Autolib.Reporter.Type
import qualified Autolib.Reporter.Checker as C
import qualified Machine.Numerical.Config 

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size
import Text.XML.HaXml.Haskell2Xml

import Data.Typeable

data Property = Det
	      | Final_States_Are_Dead
	      | Sane
              | Max_Working_Alphabet_Size Int
              | Max_Number_Of_States Int
    deriving ( Eq, Ord, Typeable )

{-! for Property derive: Reader, ToDoc, Haskell2Xml !-}

instance Condition  Property ( Turing Char Int ) where
    condition p a = C.run ( Turing.Property.check p ) a

instance Explain Property where
    explain p = C.condition ( Turing.Property.check p )

instance Machine.Numerical.Config.Check Property ( Turing Char Int ) where
    check p m = C.investigate ( Turing.Property.check p ) m

check :: Property -> C.Type  ( Turing Char Int )
check p = case p of
	 Sane ->  C.Make 
            { C.nametag = ""
            , C.condition = 
                 text "Der Turingmaschine soll konsistent definiert sein."
            , C.investigate = Turing.Check.check
            }
         Det ->  C.Make 
            { C.nametag = ""
            , C.condition = 
                 text "Die Turingmaschine soll deterministisch sein."
            , C.investigate = Turing.Check.deterministisch
            }
         Final_States_Are_Dead -> C.Make
            { C.nametag = ""
            , C.condition = 
                 text "Es gibt keinen Übergang von einem Finalzustand aus."
            , C.investigate = Turing.Check.final_states_are_dead
            }
         Max_Working_Alphabet_Size s -> C.Make
            { C.nametag = ""
            , C.condition = 
                 text "maximale Arbeitsalphabetgröße" <+> toDoc s
            , C.investigate = \ m -> 
                 when ( size ( arbeitsalphabet m ) > s )
                      $ reject $ text "Arbeitsalphabet zu groß."
            }
         Max_Number_Of_States s -> C.Make
            { C.nametag = ""
            , C.condition = 
                 text "maximale Zustandszahl" <+> toDoc s
            , C.investigate = \ m -> 
                 when ( size ( zustandsmenge m ) > s )
                      $ reject $ text "Zustandszahl zu groß."
            }

