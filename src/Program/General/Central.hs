{-# OPTIONS -fglasgow-exts #-}

module Program.General.Central where

import Program.General.Environment
import Program.General.Program
import Program.General.Class

-- import qualified Program.Array.Roll as R
-- import qualified Program.Array.Config as F

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Reader
import qualified Challenger as C
import Inter.Types
import Inter.Quiz
import Autolib.Size
import Autolib.Util.Zufall ( repeat_until )

import Data.Typeable
import Data.Maybe ( isNothing, isJust )

data Program_General = Program_General deriving ( Eq, Ord, Show, Read, Typeable )

instance ( Eq val, Value val, Program.General.Class.Class st val 
         , Reader ( Environment val )
         , Typeable st, Typeable val
         )
    => C.Partial Program_General ( Program st , Environment val ) ( Environment val ) where

    describe _ (p, e) = vcat
        [ text "Deklarieren und initialisieren Sie die Variablen,"
	, text "so daß sich nach Ausführung des Programmes"
	, nest 4 $ toDoc p
	, text "die folgende Belegung ergibt:"
	, nest 4 $ toDoc e
	]

    initial _ (p, e) = e -- Program.Array.Environment.example

    total _ ( p , target) start = do
        inform $ text "Ich führe das Programm aus:"
        actual <- nested 4 $ Program.General.Central.execute start p
	inform $ vcat
	    [ text "Die resultierende Belegung ist:"
	    , nest 4 $ toDoc actual
	    ]
	inform $ text "Ich vergleiche mit der Aufgabenstellung:"
	nested 4 $ must_be_equal target actual

execute :: Class st val
        => Environment val -> Program st -> Reporter ( Environment val )
execute start ( Program ss ) = foldM single start ss



{-
make_fixed :: Make
make_fixed = direct 
       Program_General	
       ( Program.Array.Program.s , Program.Array.Environment.example )

make_quiz :: Make
make_quiz = quiz Program_General F.example


instance Generator 
	     Program_General 
	     F.Config 
	     ( Environment, Program, Environment ) where
    generator p conf key = 
        R.roll conf `repeat_until` nontrivial conf

nontrivial conf (_, Program sts , final) = not $ or $ do
    let bnd = ( 0 , fromIntegral $ F.max_data_size conf )
    ps <- [] : map return ( patches final bnd )
    return $ matches ( final ,  Program $ ps ++ sts , final )

matches ( start, prog, final ) = 
    isJust $ result $ C.total Program_General ( prog, final ) start


            

instance Project
	     Program_General 
	     ( Environment, Program, Environment )
	     ( Program, Environment ) where
    project _ ( start, p, final ) = ( p, final )

-}
