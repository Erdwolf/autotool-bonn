module Convert.Main where

--  $Id$

import Convert.Type

import Inter.Types
import Inter.Quiz
import Autolib.ToDoc
import Autolib.Hash
import Autolib.Util.Seed
import qualified Challenger as C

import Data.Typeable
import Autolib.Reporter

data Convert_To_NFA = Convert_To_NFA deriving ( Eq, Ord, Show, Read, Typeable )

instance C.Partial Convert_To_NFA Convert ( NFA Char Int ) where

    describe Convert_To_NFA con = vcat
        [ text "Gesucht ist ein endlicher Automat,"
	, text "der die Sprache" <+> text "TODO" <+> text "akzeptiert"
	, text "und folgende Eigenschaften hat:"
	, indent 4 $ toDoc $ wanted con
        ]

    initial Convert_To_NFA con = Autolib.NFA.example

    partial Convert_To_NFA ( Convert { wanted = NFA props } ) aut = do
        inform $ text "Sind alle Eigenschaften erfüllt?"
        mapM_ ( flip test aut ) props

    total Convert_To_NFA ( Convert { input = i } ) aut = do
        inform $ text "Akzeptiert der Automat die richtige Sprache?"
        -- todo

instance C.Measure Convert_To_NFA con aut where
    measure _ _ aut = fromIntegral $ size aut

make :: Make
make = direct Convert_To_NFA 
     $ Convert { name = Nothing
	       , input = Fixed_RX $ read "a (a+b)^* b"
	       , wanted = NFA NFA.Property.example
	       }
