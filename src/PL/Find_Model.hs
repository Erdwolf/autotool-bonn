{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

module PL.Find_Model (
      make_fixed 
) where

--  $Id$

import PL.Type
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


instance Partial Find_Model Param ( Interpretation Int ) where

    describe Find_Model p = vcat
        [ text "Finden Sie f�r die Formel"
	, nest 4 $ toDoc $ formel p
	, text "ein Modell (eine Interpretation) der Gr��e"
	, nest 4 $ toDoc $ model_size p
	]

    initial Find_Model p = 
        PL.Interpretation.empty ( signatur $ formel p ) 
				( mkSet [ 1 .. model_size p ] )

    partial Find_Model p i = do
	   assert ( model_size p == cardinality ( universum $ struktur i ) )
		  $ text "Modellgr��e ist korrekt?"
	   check ( signatur $ formel p ) i

    total Find_Model p i = do
        v <- evaluate i ( formel p ) 
	inform $ text "Wert der Formel unter der Interpretation ist" <+> toDoc v
	when ( not v ) $ reject $ text "Interpretation ist kein Modell"

make_fixed :: Make
make_fixed = direct Find_Model PL.Param.example

	   