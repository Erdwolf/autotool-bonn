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

tests ps exp = sequence_ $ do p <- ps ; return $ test p exp

{-
test :: ( Symbol c, Reader [c], ToDoc [c] )
     => Property c 
     -> RX c
     -> Reporter ()
-}
test (Max_Size s) exp = do
    let g = size exp
    assert ( g <= s ) 
	   $ text "Größe des Ausdrucks" <+> parens ( toDoc g ) <+> text "ist höchstens" <+> toDoc s <+> text "?"

test (Alphabet a) exp = do
    sanity_alpha a exp

test (Simple) exp = do
    ist_einfach exp

test (Extended) exp = do
    ist_erweitert exp

test (AllowedKeys ks) exp = do
    sanity_keys ks exp

test (Max_Star_Height h) exp = do
    let sh = star_height exp
    inform $ vcat 
           [ text "Ausdruck" <+> parens ( toDoc exp )
             </> text "hat Sternhöhe" <+> toDoc sh
           , text "höchste zulässige Höhe ist"
             </> toDoc h
           ]
    when  ( sh > h ) $ reject $ text "Das ist zu hoch."

test prop exp = do
    reject $ fsep [ text "test für", toDoc prop
		  , text "noch nicht implementiert"
		  ]
