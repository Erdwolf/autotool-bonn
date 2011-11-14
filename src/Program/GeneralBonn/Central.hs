{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Program.GeneralBonn.Central where

import Program.GeneralBonn.Environment
import Program.GeneralBonn.Program
import Program.GeneralBonn.Class

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

instance ( ToDoc val, Reader val, Eq val
         , Program.GeneralBonn.Class.Class p st val 
         , Typeable st
         )
    => C.Partial p ( Program st , Environment val ) ( Environment val ) where

    describe _ (p, e) = vcat
        [ text "Deklarieren und initialisieren Sie die Variablen,"
	, text "so daß sich nach Ausführung des Programmes"
	, nest 4 $ toDoc p
	, text "die folgende Belegung ergibt:"
	, nest 4 $ toDoc e
	]

    initial _ (p, e) = e -- Program.Array.Environment.example

    total tag ( p , target) start = do
        inform $ text "Ich führe das Programm aus:"
        actual <- nested 4 $ Program.GeneralBonn.Class.execute tag start p
	inform $ vcat
	    [ text "Die resultierende Belegung ist:"
	    , nest 4 $ toDoc actual
	    ]
	inform $ text "Ich vergleiche mit der Aufgabenstellung:"
	nested 4 $ must_be_equal target actual



make_fixed :: Class p st val, Reader ( Environment val ), ToDoc ( Environment val ) =>
              p -> Make
make_fixed p = direct 
       p
       ( example p )

{-
make_quiz :: Make
make_quiz = quiz Program.GeneralBonn F.example


instance Generator 
	     Program.GeneralBonn 
	     F.Config 
	     ( Environment, Program, Environment ) where
    generator p conf key = 
        R.roll conf `repeat_until` nontrivial conf

nontrivial conf (_, Program sts , final) = not $ or $ do
    let bnd = ( 0 , fromIntegral $ F.max_data_size conf )
    ps <- [] : map return ( patches final bnd )
    return $ matches ( final ,  Program $ ps ++ sts , final )

matches ( start, prog, final ) = 
    isJust $ result $ C.total Program.GeneralBonn ( prog, final ) start


            

instance Project
	     Program.GeneralBonn 
	     ( Environment, Program, Environment )
	     ( Program, Environment ) where
    project _ ( start, p, final ) = ( p, final )

-}
