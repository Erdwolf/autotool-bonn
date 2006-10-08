{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}


module PL.ToDoc where

import PL.Data

import Autolib.ToDoc


import Autolib.TES.Identifier

instance ToDoc Formel where
    toDocPrec p ( Operation op xs ) = docParen ( p > 0 ) $ case xs of
        [x]   -> toDoc op <+> toDocPrec 0 x
	[x,y] -> toDocPrec 1 x <+> toDoc op <+> toDocPrec 1 y
    toDocPrec p ( Quantified q x f ) = docParen ( p > 0 ) $
        toDoc q <+> toDoc x <+> text "." <+> toDoc f
    toDocPrec p ( Predicate r xs ) = 
        toDoc r <+> parens ( sepBy comma $ map toDoc xs  )


instance ToDoc Operator where
    toDoc op = case op of
        Not -> text "not"
	And -> text "&&"
	Or -> text "||"
	Iff -> text "<=>"
	Implies -> text "=>"

instance ToDoc Quantor where
    toDoc q = case q of
        Forall -> text "forall"
	Exists -> text "exists"

instance ToDoc Term where
    toDoc ( Variable v ) = toDoc v
    toDoc ( Apply f xs ) = 
        toDoc f <+> parens ( sepBy comma $ map toDoc xs )



