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

import Data.Typeable

newtype T t = T t deriving (Typeable)

instance OrderScore (T t) where
    scoringOrder _ = Increasing

instance Show t => Show (T t) where
    show (T t) = show t

instance Read t => Read (T t) where
    readsPrec d s = [(T t, s') | (t, s') <- readsPrec d s]

instance Algebraic tag a 
    => Partial (T tag) ( Algebraic.Instance.Type a ) ( Exp a ) where

    report (T p) i = do
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

    initial (T p) i = some_formula p i

    partial (T p) i b = do
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

    total (T p) i b = do
        v <- evaluate p b
        eq <- equivalent p ( target i ) v
        when ( not eq ) $ reject $ text "nicht äquivalent"

make :: Algebraic tag m
     => tag -> Make
make tag = direct (T tag) $ default_instance tag


