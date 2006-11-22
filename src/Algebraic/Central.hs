{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances  -fallow-incoherent-instances -fallow-undecidable-instances #-}

module Algebraic.Central where

import Algebraic.Instance
import Algebraic.Class
import Expression.Op

import Challenger.Partial
import Inter.Types


import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Reporter.Set
import Autolib.Size
import Autolib.Set


instance Algebraic tag a 
    => Partial tag ( Algebraic.Instance.Type a ) ( Exp a ) where

    report p i = do
        inform $ vcat
            [ text "Gesucht ist ein Ausdruck (Term) mit dieser Bedeutung:"
            , nest 4 $ case description i of
                Nothing -> toDoc $ target i
                Just cs -> text cs
    		, text "der nur diese Symbol enthält:"
    		, nest 4 $ toDoc $ operators i
            , text "und maximal die Größe" <+> toDoc (max_size i) <+> text "hat."
    	    ]
	present p $ target i

    initial p i = some_formula p i

    partial p i b = do
        inform $ vcat
	       [ text "Die Baumstruktur Ihrer Einsendung ist:"
	       , nest 4 $ draw b
	       , text ""
	       ]
        Autolib.Reporter.Set.subeq
	    ( parens $ text "benutzte Operatoren" , syms b )
	    ( parens $ text "erlaubte Operatoren" 
	    , mkSet $ flatten $ operators i 
	    )
        let s = size b
        inform $ text "Die Größe Ihres Ausdrucks ist" <+> toDoc s
        when ( s > max_size i ) $ reject $ text "Das ist zuviel."

    total p i b = do
        v <- evaluate p b
        eq <- equivalent p ( target i ) v
        when ( not eq ) $ reject $ text "nicht äquivalent"

make :: Algebraic tag m
     => tag -> Make
make tag = direct tag $ default_instance tag


