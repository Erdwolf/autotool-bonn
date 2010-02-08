module PL.Find_Model (
      make_fixed 
) where

--  $Id$

import PL.Type
import PL.Tree
import PL.Param
import PL.Signatur
import PL.Struktur
import PL.Interpretation
import PL.Semantik


import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Set
import Autolib.Size

import Inter.Types

import Data.Typeable

data Find_Model = Find_Model deriving ( Show, Read, Typeable )

instance OrderScore Find_Model where
    scoringOrder _ = Increasing

instance Partial Find_Model Param ( Interpretation Int ) where

    report Find_Model p = do
        inform $ vcat
            [ text "Finden Sie für die Formel"
	    , nest 4 $ toDoc $ formel p
	    , text "ein Modell (eine Interpretation) der Größe"
	    , nest 4 $ toDoc $ model_size p
	    ]
        peng $ formel p

    initial Find_Model p = 
        PL.Interpretation.empty ( signatur $ formel p ) 
				( mkSet [ 1 .. model_size p ] )

    partial Find_Model p i = do
	   assert ( model_size p == cardinality ( universum $ struktur i ) )
		  $ text "Modellgröße ist korrekt?"
	   check ( signatur $ formel p ) i

    total Find_Model p i = do
        v <- evaluate_top i ( formel p ) 
	inform $ text "Wert der Formel unter der Interpretation ist" <+> toDoc v
	when ( not v ) $ reject $ text "Interpretation ist kein Modell"

make_fixed :: Make
make_fixed = direct Find_Model PL.Param.example

	   
