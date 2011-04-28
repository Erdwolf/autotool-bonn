{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, FlexibleInstances #-}
module Collatz.Inverse where

--  $Id$

import qualified Collatz.Parameter as P
import Collatz.Config
import Collatz.Roll

import Inter.Types
import Inter.Quiz
import Autolib.ToDoc
import Autolib.Hash
import Autolib.Util.Seed
import qualified Challenger as C

import Data.Typeable
import Autolib.Reporter

data Collatz_Inverse = Collatz_Inverse deriving ( Eq, Ord, Show, Read, Typeable )

instance OrderScore Collatz_Inverse where
    scoringOrder _ = None -- ?

instance C.Partial Collatz_Inverse P.Parameter Integer where

    report Collatz_Inverse p = do
        inform $ vcat
	       [ text "Gesucht ist eine Startzahl, deren Collatz-Folge"
	       , text "diese Parameter hat:" <+> toDoc p
	       ]

    initial Collatz_Inverse p = 27

    total Collatz_Inverse p x = do
        -- vorsicht: hier könnte max [] = undefined vorkommen
        -- aber wegen Parameter deriving Eq werden zuerst die längen verglichen
        -- und die stimmen dann eben nicht, weil nur instanzen
        -- mit längen > 0 gewürfelt werden (hoffentlich)
	assert ( p == P.compute x )
	       $ text "angegebene Zahl ist korrekt?"

instance C.Measure Collatz_Inverse P.Parameter Integer where
    measure Collatz_Inverse p x = 1

make :: Make
make = direct Collatz_Inverse P.one




instance Generator Collatz_Inverse Config ( Integer, P.Parameter ) where
    generator p conf key = do
        seed $ fromIntegral $ hash key
	roll conf

instance Project Collatz_Inverse ( Integer, P.Parameter ) P.Parameter where
    project p ( _, q ) = q

qmake :: Make
qmake = quiz Collatz_Inverse rc



