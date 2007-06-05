module Code.Nocode where


import Code.Formal

import Challenger.Partial
import Inter.Types

import Autolib.Reporter
import Autolib.Set
import Autolib.ToDoc

import Data.Typeable

data Nocode = Nocode deriving ( Read, Show, Typeable )

instance ( Ord a, Eq b ) => 
    Partial Nocode ( FiniteMap a [b] ) ( [a],[a] )  where
    describe Nocode f = vcat
        [ text "Beweisen Sie, daß die Abbildung"
	, nest 4 $ text "f =" <+> toDoc f
	, text "kein Code ist:" 
	, text "Geben Sie ein Wortpaar (u,v) mit u /= v und f(u)=f(v) an."
	]
    initial Nocode f = 
        let ks = keysFM f	
	in  ( take 2 ks, take 2 $ reverse ks ) 
    partial Nocode f (u,v) = do
	when ( u == v ) $ reject 
             $ text "die beiden Wörter sollen verschieden sein."
    total   Nocode f (u,v) = do
        fu <- morph f u
	fv <- morph f v
	when ( fu /= fv ) $ reject 
	     $ text "die beiden Bilder sollen gleich sein."

morph f u = do
    inform $ text "Das Bild von" <+> toDoc u 
    fu <- sequence $ do
        x <- u
        case lookupFM f u of
	    Nothing -> reject 
                $ text "der Buchstabe" <+> toDoc x <+> text "hat kein Bild."
	    Just fx -> fx
    inform $ text "ist" <+> toDoc fu
    return fu

instance  ( Ord a, Eq b ) =>
    Measure Nocode ( FiniteMap a [b] ) ( [a],[a] ) where
    measure Nocode f (u,v) = 
        fromIntegral $ length u + length v

make_fixed = direct Nocode $ 
    listToFM [ ('A', "00"), ('B',"001"),('C',"10"),('D',"01") ]

