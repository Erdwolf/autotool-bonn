{-# OPTIONS -fglasgow-exts #-}

module Program.Array.Central where

import Program.Array.Program
import Program.Array.Environment
import Program.Array.Semantics

import Autolib.Reporter
import Autolib.ToDoc
import qualified Challenger as C
import Inter.Types
import Autolib.Size

import Data.Typeable

data Program_Array = Program_Array deriving ( Eq, Ord, Show, Read, Typeable )

instance C.Partial Program_Array ( Program, Environment ) Environment where

    describe _ (p, e) = vcat
        [ text "Deklarieren und initialisieren Sie die Variablen,"
	, text "so daß sich nach Ausführung des Programmes"
	, nest 4 $ toDoc p
	, text "die folgende Belegung ergibt:"
	, nest 4 $ toDoc e
	]

    initial _ (p, e) = Program.Array.Environment.example

    total _ ( p , target) start = do
        actual <- Program.Array.Semantics.execute start p
	must_be_equal target actual

make :: Make
make = direct 
       Program_Array	
       ( Program.Array.Program.s , Program.Array.Environment.example )
