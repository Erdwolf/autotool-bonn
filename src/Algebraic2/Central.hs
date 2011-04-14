{-# LANGUAGE DeriveDataTypeable #-}
module Algebraic2.Central where

import Algebraic2.Instance
import Algebraic2.Class
import Expression.Op

import Challenger.Partial
import Inter.Types

import qualified Autolib.TES.Binu as B
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter
import Autolib.Reporter.Set
import Autolib.Size
import Autolib.Set
import Autolib.FiniteMap

import Data.Typeable

newtype T t = T t deriving (Typeable)

instance OrderScore (T t) where
    scoringOrder _ = Increasing

instance Show t => Show (T t) where
    show (T t) = show t

instance Read t => Read (T t) where
    readsPrec d s = [(T t, s') | (t, s') <- readsPrec d s]

instance Algebraic tag context a 
    => Partial (T tag) ( Algebraic2.Instance.Type context a ) ( Exp a ) where

    report (T p) i = do
        introduce p $ context i

        inform $ vcat
            [ text "Gesucht ist ein Ausdruck (Term) mit dieser Bedeutung:"
            , nest 4 $ case description i of
                Nothing -> toDoc $ target i
                Just cs -> text cs
            , text "Der Ausdruck soll höchstens die Größe" 
		   <+> toDoc (max_size i) <+> text "haben."
    	    , text "Sie dürfen diese Symbole benutzen"
    	    , nest 4 $ nice $ operators i
	    , text "und diese vordefinierten Konstanten:"
	    , nest 4 $ case fmToList $ predefined i of
		   [] -> parens $ text "keine"
		   bzs -> vcat $ do
		         ( b, z ) <- bzs
			 return $ hsep [ toDoc b, text "=", toDoc z ]
    	    ]
	present p $ target i

    initial (T p) i = some_formula p i

    partial (T p) i b = do
        inform $ vcat
	       [ text "Die Baumstruktur Ihrer Einsendung ist:"
	       , nest 4 $ draw b
	       , text ""
	       ]
        silent $ Autolib.Reporter.Set.subeq
	    ( parens $ text "benutzte Operatoren" , syms b )
	    ( parens $ text "erlaubte Operatoren" 
	    , mkSet $ flatten $ operators i 
	    )

	silent $ Autolib.Reporter.Set.subeq
	    ( parens $ text "benutzte Konstanten" , vars b )
	    ( parens $ text "erlaubte Konstanten" 
	    , mkSet $ keysFM $ predefined i
	    )

        let s = size b
        inform $ text "Die Größe Ihres Ausdrucks ist" <+> toDoc s
        when ( s > max_size i ) $ reject $ text "Das ist zuviel."

    total (T p) i b = do
        v <- evaluateC p ( context i ) ( predefined i ) b
        eq <- equivalent p ( target i ) v
        when ( not eq ) $ reject $ text "nicht äquivalent"

make :: Algebraic tag context m
     => tag -> Make
make tag = direct (T tag) $ default_instance tag


instance ( Reader c, ToDoc c ) => Nice ( B.Binu c ) where
    nice b = vcat
        [ text "zweistellige :" <+> toDoc ( B.binary b )
	, text "einstellige  :" <+> toDoc ( B.unary  b )
	, text "nullstellige :" <+> toDoc ( B.nullary b )
	]


