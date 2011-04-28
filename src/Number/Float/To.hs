{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Number.Float.To

( make_fixed
, make_quiz
) 

where

import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reporter

import Inter.Types
import Inter.Quiz
import Data.Typeable
import Data.Ratio
import Autolib.Size
import Autolib.Xml
import System.Random

import Number.Wert
import Number.Float.Data
import Number.Float.Near
import Number.Float.Config as C

import qualified Number.Float.From () -- only Rational Container

-------------------------------------------------------------------------------

data To_Float = To_Float deriving ( Show, Read, Typeable )

instance OrderScore To_Float where
    scoringOrder _ = None

instance Partial To_Float (Config, Rational) Zahl where

    describe To_Float (c, x) = vcat
	   [ text "Welche Gleitkommazahl mit diesen Eigenschaften"
           , nest 4 $ toDoc c
           , text "ist eine gute Näherung für" <+> toDoc x <+> text "?"
	   ]

    initial To_Float (c, x) = 
        Number.Float.Data.example

    partial To_Float (c, x) z = do
        conforms c  z

    total To_Float (c, x) z = do
        near c x z

-------------------------------------------------------------------------------

make_fixed :: Make
make_fixed = direct To_Float ( C.example , 4 % 5 :: Rational )

make_quiz :: Make
make_quiz = quiz To_Float C.example

instance Generator To_Float Config ( Config, Rational ) where
    generator _ c key = do
        let rng = fromIntegral (C.basis c) ^ max_stellen_exponent c
        en <- randomRIO ( 1, rng )
        de <- randomRIO ( 1, rng )
        return ( c, en % de )

instance Project To_Float (Config, Rational) (Config, Rational) where
    project To_Float (c, x) = (c, x)






