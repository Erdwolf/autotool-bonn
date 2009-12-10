module Lambda.Derive where

import Lambda.Type
import Lambda.Tree ( peng )
import Lambda.Quiz
import Lambda.Derive.Instance as I
import Lambda.Derive.Config as C
import Lambda.Step

import Challenger.Partial
import Inter.Types
import Inter.Quiz
import Autolib.ToDoc
import Autolib.Reporter
import Data.Typeable

data Lambda_Derive = Lambda_Derive 
    deriving ( Read, Show, Typeable )

instance Measure Lambda_Derive I.Type [ Int ] where
    measure p inst xs = fromIntegral $ length xs

instance Partial Lambda_Derive I.Type [ Int ] where
    report p inst = do
        let step_info = case steps inst of
               Nothing -> empty
               Just s  -> text "der Länge" <+> toDoc s
        inform $ vcat
            [ fsep [ text "Gesucht ist eine Ableitung"
                   , step_info, text ", die"
                   ]
            , nest 4 $ toDoc $ from inst
            ]
        peng $ from inst        
        inform $ vcat 
            [ text "überführt in"
            , nest 4 $ toDoc $ to inst
            ]
        peng $ to inst
        inform $ vcat
            [ text "Jeder Ableitungsschritt wird durch eine Nummer"
            , text "aus einer Liste von möglichen Schritten bezeichnet."
            ]
        
    initial p inst = [ 0 ]

    partial p inst xs = do
        result <- derive ( from inst ) xs
        return ()

    total p inst xs = do
        result <- silent $ derive ( from inst ) xs
        check_length inst xs
        check_result inst result


check_length inst xs = do
    case steps inst of
        Nothing -> return ()
        Just s -> assert ( s == length xs )
                         $ text "Ableitungslänge korrekt"
                           
check_result inst t = do
    assert ( to inst == t )
           $ text "Ableitungsergebnis korrekt?"


make_fixed :: Make
make_fixed = direct Lambda_Derive I.example                    

instance Generator Lambda_Derive C.Type I.Type where
    generator p = Lambda.Quiz.generator

instance Project Lambda_Derive I.Type I.Type where
    project p = id


make_quiz :: Make
make_quiz = quiz Lambda_Derive C.example

