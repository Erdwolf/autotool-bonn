{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses #-}
module Lambda.Backward_Join where

import Lambda.Type
import Lambda.Tree ( peng )
import qualified Lambda.Backward_Join.Instance as I
import qualified Lambda.Backward_Join.Solution as S
import Lambda.Step
import Lambda.Alpha

import Challenger.Partial
import Inter.Types
import Autolib.ToDoc
import Autolib.Size
import Autolib.Reporter
import Data.Typeable

data Lambda_Backward_Join = Lambda_Backward_Join 
    deriving ( Read, Show, Typeable )

instance OrderScore Lambda_Backward_Join where
    scoringOrder _ = Increasing

instance Measure Lambda_Backward_Join  I.Type S.Type where
    measure p inst sol = fromIntegral $ size $ S.start sol

instance Partial Lambda_Backward_Join I.Type S.Type where
    report p inst = do
        inform $ vcat
            [ text "gesucht ist ein Term t"
	    , text "mit t ->^*" <+> toDoc ( I.left_goal inst )
	    , text "und t ->^*" <+> toDoc ( I.right_goal inst )
            ]
        inform $ vcat
            [ text "Sie sollen jeweils auch die Ableitungen angeben."
	    , text "Dabei wird jeder Schritt durch eine Nummer"
            , text "aus einer Liste von möglichen Schritten bezeichnet."
            ]
        
    initial p inst = S.example

    total p inst sol = do
        verify_derivation "linke"
	    ( S.start sol ) ( S.left_steps sol ) ( I.left_goal inst )  
        verify_derivation "rechte"
	    ( S.start sol ) ( S.right_steps sol ) ( I.right_goal inst ) 

verify_derivation tag start steps goal = do
    inform $ fsep [ text "prüfe", text tag, text "Ableitung" ]
    final <- nested 4 $ derive start steps
    assert ( Lambda.Alpha.convertible final goal ) 
	   $ vcat [ text "ist alpha-konvertierbar zu", nest 4 $ toDoc goal ]

make_fixed :: Make
make_fixed = direct Lambda_Backward_Join I.example                    




