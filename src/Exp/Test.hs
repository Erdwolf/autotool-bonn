module Exp.Test where

--  $Id$

import Exp.Property

import Autolib.Exp
import Autolib.Exp.Einfach
import Autolib.Exp.Sanity

import Autolib.Reporter
import Autolib.Reporter.Set
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size
import Autolib.Symbol

test :: ( Symbol c, Reader [c], ToDoc [c] )
     => Property c 
     -> RX c
     -> Reporter ()

test (Min_Size s) exp = do
    assert ( size exp >= s ) 
	   $ text "Gr��e des Ausdrucks ist wenigstens" <+> toDoc s <+> text "?"
test (Max_Size s) exp = do
    assert ( size exp <= s ) 
	   $ text "Gr��e des Ausdrucks ist h�chstens" <+> toDoc s <+> text "?"

test (Alphabet a) exp = do
    sanity_alpha a exp

test (Simple) exp = do
    ist_einfach exp

test (Extended) exp = do
    ist_erweitert exp

test prop exp = do
    reject $ fsep [ text "test f�r", toDoc prop
		  , text "noch nicht implementiert"
		  ]
