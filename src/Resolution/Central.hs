module Resolution.Central ( make_fixed, make_quiz ) where

import Resolution.Data
import Resolution.Action
import Resolution.Execute
import Resolution.Config
import Resolution.Roll

import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reporter hiding ( execute )

import Inter.Types
import Inter.Quiz
import Data.Typeable

data Resolution = Resolution deriving ( Show, Read, Typeable )

instance Partial Resolution ( [Clause], Clause ) [ Action ] where

    describe _  ( cs, target ) = vcat
        [ text "Gesucht ist eine Resolutions-Ableitung für die Klausel"
	, nest 4 $ toDoc $ target 
	, text "aus der Klauselmenge"
	, nest 4 $ toDoc $ State cs
	]

    initial _ ( cs, target ) = 
        [ Resolution.Action.example ]
        
    partial _ ( cs, target ) prog = do
        return ()

    total _ ( cs, target ) prog = do
        State cs' <- foldM execute ( State cs ) prog
        assert ( last cs' == target ) 
               $ text "letzte abgeleitete Klausel ist Zielklausel?" 
               
        

make_fixed :: Make
make_fixed = direct Resolution 
             ( [ read "x || ! y", read "y || ! z", read "x || z", read "! x" ] :: [ Clause ]
             , read "False" :: Clause
             )

instance Generator Resolution Config ( [Clause], Clause ) where
    generator _ conf key = Resolution.Roll.medium conf

instance Project Resolution ( [Clause], Clause ) ( [Clause], Clause ) where
    project _ = id

make_quiz :: Make
make_quiz = quiz Resolution Resolution.Config.example
