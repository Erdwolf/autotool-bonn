{-# LANGUAGE DeriveDataTypeable #-}
module Palindrom.Plain where

--  $Id$

import Inter.Types
import Inter.Quiz
import Autolib.ToDoc
import Autolib.Hash
import Autolib.Util.Seed
import qualified Challenger as C

import Data.Typeable
import Autolib.Reporter

data Palindrom = Palindrom deriving ( Eq, Ord, Show, Read, Typeable )

instance OrderScore Palindrom where
    scoringOrder _ = Decreasing

instance C.Partial Palindrom Int Integer where

    report Palindrom e = do
        inform $ vcat
	       [ text "Gesucht ist eine Zahl x,"
	       , text "so daß die Dezimaldarstellung von"
		 <+> text "x^" <> toDoc e
		 <+> text "ein Palindrom ist,"
	       , text "aber die Dezimaldarstellung von x kein Palindrom ist."
	       , text ""
	       , parens $ text "Highscore-Wertung: Quersumme von x"
	       ]

    initial Palindrom e = 13

    partial Palindrom e x = do
        assert ( not $ ispali x )
	       $ text "dezimal(x) ist kein Palindrom?"

    total Palindrom e x = do
        let xe = x ^ e
        inform $ text ( "dezimal (x^" ++ show e ++ ") =" ) <+> toDoc xe
        assert ( ispali xe )
	       $ text "ist Palindrom?"

instance C.Measure Palindrom Int Integer where
    measure Palindrom e x = 
        fromIntegral $ sum $ map ( read . return ) $ show x

ispali :: Integer -> Bool
ispali x = let cs = show x 
	   in  cs == reverse cs

make :: Make
make = direct Palindrom ( 2 :: Int )


