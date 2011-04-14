{-# LANGUAGE DeriveDataTypeable #-}
module Code.Nocode where


import Code.Formal

import Challenger.Partial
import Inter.Types
import Inter.Quiz

import Autolib.Reporter
import Autolib.FiniteMap
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Util.Zufall

import Data.Typeable
import Data.List

data Nocode = Nocode deriving ( Read, Show, Typeable )

instance OrderScore Nocode where
    scoringOrder _ = None -- ?

instance 
    Partial Nocode [( Char, String)] ( String,String )  where
    describe Nocode f = vcat
        [ text "Beweisen Sie, daß die Abbildung"
	, nest 4 $ text "f =" <+> toDoc ( listToFM f )
	, text "kein Code ist:" 
	, text "Geben Sie ein Wortpaar (u,v) mit u /= v und f(u) = f(v) an."
	]
    initial Nocode f = 
        let ks = keysFM $ listToFM f	
	in  ( take 2 ks, take 2 $ reverse ks ) 
    partial Nocode f (u,v) = do
	when ( u == v ) $ reject 
             $ text "die beiden Wörter sollen verschieden sein."
    total   Nocode f0 (u,v) = do
        let f = listToFM f0
        fu <- morph f u
	fv <- morph f v
	when ( fu /= fv ) $ reject 
	     $ text "die beiden Bilder sollen gleich sein."

morph :: ( ToDoc [b], ToDoc [a], ToDoc a, Ord a )
      => FiniteMap a [b] -> [a] -> Reporter [b]
morph f u = do
    inform $ text "Das Bild von" <+> toDoc u 
    fu <- fmap concat $ sequence $ do
        x <- u
        return $ case lookupFM f x of
	    Nothing -> reject 
                $ text "der Buchstabe" <+> toDoc x <+> text "hat kein Bild."
	    Just fx -> return fx
    inform $ text "ist" <+> toDoc fu
    return fu

instance ( ToDoc a, Reader a, Reader [a], Ord a
	 , ToDoc b, Reader b, Eq b 
	 ) =>
    Measure Nocode [( a, [b] )] ( [a],[a] ) where
    measure Nocode f (u,v) = 
        fromIntegral $ length u + length v

make_fixed = direct Nocode $ 
    [ ('A', "00"), ('B',"001"),('C',"10"),('D',"01") ]

-----------------------------------------------------------------------

make_quiz = quiz Nocode ()

roll_nocode :: IO ([ String ], [String])
roll_nocode =  
   do ws <- sequence $ replicate 5 $ do
	 l <- randomRIO ( 2, 5 )
	 someIO "01" l
      let ce = code_counter_examples ws
      return ( ws, take 1 ce )
   `repeat_until` \ ( ws, ce) -> 
         not (null ce) 
      && length (head ce) >= 15
      && length (nub $ concat ce) > 1
    
instance Generator Nocode ()  ([ String ], [String]) where
    generator _ conf key = roll_nocode

instance Project Nocode  ([ String ], [String]) [(Char,String)]  where
    project _ ( ws, ce ) = 
        zip [ 'A' .. ] ws

