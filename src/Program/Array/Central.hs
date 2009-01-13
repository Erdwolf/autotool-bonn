{-# OPTIONS -fglasgow-exts #-}

module Program.Array.Central where

import Program.General.Program
import Program.General.Environment

-- import Program.Array.Semantics
import qualified Program.Array.Roll as R
import qualified Program.Array.Config as F

import Autolib.Reporter
import Autolib.ToDoc
import qualified Challenger as C
import Inter.Types
import Inter.Quiz
import Autolib.Size
import Autolib.Util.Zufall ( repeat_until )

import Data.Typeable
import Data.Maybe ( isNothing, isJust )

{-

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
        inform $ text "Ich führe das Programm aus:"
        actual <- nested 4 $ Program.Array.Semantics.execute start p
	inform $ vcat
	    [ text "Die resultierende Belegung ist:"
	    , nest 4 $ toDoc actual
	    ]
	inform $ text "Ich vergleiche mit der Aufgabenstellung:"
	nested 4 $ must_be_equal target actual

execute :: Environment -> Program -> Reporter Environment
execute start ( Program ss ) = foldM single start ss

make_fixed :: Make
make_fixed = direct 
       Program_Array	
       ( Program.Array.Program.s , Program.Array.Environment.example )

-}
            

