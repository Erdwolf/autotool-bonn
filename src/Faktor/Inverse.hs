{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Faktor.Inverse (
       make_fixed
     , make_quiz
) where

import Faktor.Inverse.Param

import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Util.Zufall

import Inter.Types
import Inter.Quiz

import Data.Typeable

data Inverse = Inverse deriving ( Show, Read, Typeable )

instance OrderScore Inverse where
    scoringOrder _ = None

instance Partial Inverse ( Integer, Integer ) ( Maybe Integer ) where
    describe Inverse (a, b) = vcat
        [ text "Berechnen Sie das multiplikative Inverse von"
	, nest 4 $ text $ show a
	, text "modulo"
	, nest 4 $ text $ show b
	, text "falls es existiert!"
	, text "(Eingabe: \"Nothing\" oder \"Just i\".)"
        ]
    initial Inverse (a, b) = Just a

    partial Inverse (a, b) Nothing = 
        when ( inverse a b /= Nothing ) $ reject $ 
	     text "Sie behaupten, es gäbe kein Inverses, aber es gibt eins!"
    partial Inverse (a, b) (Just i) = do
        when ( abs i > b ) $ reject $ 
	     text $ "Der Betrag des Inversen soll höchstens " ++ show (b-1) ++ " sein!"

        inform $ text "Ja."

    total Inverse (a, b) Nothing = do
        when ( inverse a b == Nothing ) $ inform $ text "Ja."
    total Inverse (a, b) (Just i) = do
        let m = a * i
        let (q,r) = divMod m b
        inform $ fsep 
               [ text "Ist", toDoc a, text "*" , toDoc i 
	       , text "=", toDoc m , text "= 1 mod" , toDoc b , text "?" 
	       ]
        let zeige = foldl1 (++) 
		  [ show m , " = " , show q , " * " , show b , " + " , show r ]
        if  r == 1
            then inform $ text $ "Ja, denn " ++ zeige
            else reject $ text $ "Nein, denn " ++ zeige

instance Measure Inverse ( Integer, Integer ) ( Maybe Integer ) where
    measure Inverse ( a, b ) Nothing = 0
    measure Inverse ( a, b ) (Just i) = i
        
instance Generator Inverse Param ( Integer, Integer , Integer ) where
    generator _ conf key = 
        do a <- randomRIO ( von conf , bis conf )
           b <- randomRIO ( von conf , bis conf )
	   p <- randomRIO ( 1 , 100 :: Integer )
           return (a, b, p)
        `repeat_until` \ (a,b,p) -> 
	    or [ p > hat_inverses_in_faellen_von_hundert conf
	       , inverse a b /= Nothing 
	       ]

instance Project Inverse ( Integer, Integer , Integer ) 
                         ( Integer , Integer ) where
    project Inverse (a,b,_) = (a,b)


make_quiz :: Make
make_quiz = quiz Inverse Faktor.Inverse.Param.p

make_fixed :: Make
make_fixed = direct Inverse ( 2431 :: Integer, 2415 :: Integer )

-------------------------------------------------------------------------------

ext_eu :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
       -> ( Integer , Integer , Integer )
ext_eu g 0 s t _ _ = (g,s,t)
ext_eu a b s t u v = let (q,r) = divMod a b
		     in ext_eu b r u v (s-q*u) (t-q*v)

inverse :: Integer -> Integer -> Maybe Integer
inverse a b = let (g,s,t) = ext_eu a b 1 0 0 1
	      in if g == 1 then Just s else Nothing

