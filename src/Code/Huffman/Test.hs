module Code.Huffman.Test

( isoptimalprefix
)

where

--  $Id$

import Code.Type
import Code.Huffman.LR
import Code.Huffman.Make
import Code.Measure

import Autolib.Util.Sort
import Data.FiniteMap
import Autolib.Reporter
import Autolib.ToDoc


isoptimalprefix ::  ( ToDoc [b], ToDoc [a], ToDoc a, Ord a, Eq b )
      => Frequency a
      -> Code a b
      -> Reporter ()
isoptimalprefix freq code = do
    inform $ vcat 
	   [ text "Ist der Code" 
	   , nest 4 $ toDoc code
	   , text "ein optimaler Pr�fix-Code f�r die Verteilung"
	   , nest 4 $ toDoc freq
	   , text "?"
	   ]
    let mcode = measure freq code
    inform $ text "Der Code hat das Gesamtgewicht" <+> toDoc mcode

    let huff = make freq
	mhuff = measure freq huff

    when ( mcode > mhuff ) $ reject
	   $ text "Das ist zu gro�."
    inform $ text "Das ist optimal."


