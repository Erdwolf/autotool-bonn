{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}


module PL.ToDoc where

import PL.Data

import Autolib.ToDoc


import Autolib.TES.Identifier

instance ToDoc Formel where
    toDocPrec p ( Operation op xs ) = case xs of
        [x]   -> toDoc op <+> toDocPrec 9 x
	[x,y] -> let q = case op of  
		       And -> 8 ; Or -> 6 ; Iff -> 4 ; Implies -> 2
		 in  docParen ( p > q )
		     $ sep [ toDocPrec (q+1) x , toDoc op <+> toDocPrec q y ]
    toDocPrec p ( Quantified q x f ) = docParen ( p > 0 ) $
        toDoc q <+> toDoc x <+> text "." <+> toDoc f
    toDocPrec p ( Predicate r xs ) = 
        toDoc r <+> parens ( sepBy comma $ map toDoc xs  )
    toDocPrec p ( Equals l r ) = docParen ( p > 8 )
	$ sep [ toDoc l , text "==", toDoc r ]


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
	Count comp n -> text "count" <+> toDoc comp <+> toDoc n

instance ToDoc Compare where
    toDoc c = text $ case c of
        Less -> "<"
	Less_Equal -> "<="
	Equal -> "=="
	Not_Equal -> "!="
	Greater_Equal -> ">="
	Greater -> ">"

instance ToDoc Term where
    toDoc ( Variable v ) = toDoc v
    toDoc ( Apply f xs ) = 
        toDoc f <+> parens ( sepBy comma $ map toDoc xs )



