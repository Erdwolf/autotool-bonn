module Collatz.Long where

--  $Id$

import qualified Collatz.Parameter as P
import Collatz.Config

import Inter.Types
import Inter.Quiz
import Autolib.ToDoc
import Autolib.Hash

import qualified Challenger as C

import Data.Typeable
import Autolib.Reporter

data Collatz_Long = Collatz_Long deriving ( Eq, Ord, Show, Read, Typeable )

instance C.Partial Collatz_Long () (Integer, Integer) where

    report Collatz_Long () (start, len)= do
        inform $ vcat
	       [ text "Gesucht ist eine lange Collatz-Folge."
               , text "Geben Sie ein Paar (Startzahl, Länge) ein."
               , text "Bewertet wird  log_2(Startzahl) - Länge."
	       ]

    initial Collatz_Long () = ( 1, 0 )

    total Collatz_Long () ( start, len) = do
	assert ( len == P.length $ P.compute start )
	       $ text "angegebene Parameter sind korrekt?"

instance C.Measure Collatz_Long Integer P.Parameter where
    measure Collatz_Long () ( start, len) = 
        -- log_2 (start) - len

make :: Make
make = direct Collatz_Long ( 27 :: Integer )

instance Generator Collatz_Long Config ( Integer, P.Parameter ) where
    generator p conf key = do
        seed $ fromIntegral $ hash $ reverse key
	roll conf

instance Project Collatz_Long ( Integer, P.Parameter ) Integer where
    project p ( i, _ ) = i

qmake :: Make
qmake = quiz Collatz_Long rc



