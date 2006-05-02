-- | möglichst lange Collatz-Folge

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

    report Collatz_Long () = do
        inform $ vcat
	       [ text "Gesucht ist eine lange Collatz-Folge."
               , text "Geben Sie ein Paar (Startzahl, Länge) ein."
               , text "Bewertet wird  Länge - log_2(Startzahl)."
	       ]

    initial Collatz_Long () = ( 7, 16 )

    total Collatz_Long () ( start, len) = do
	assert ( len == P.length ( P.compute start ) )
	       $ text "angegebene Parameter sind korrekt?"

instance C.Measure Collatz_Long () (Integer, Integer) where
    measure Collatz_Long () ( start, len) = 
        max 0 $ len - log2 start 

make :: Make
make = direct Collatz_Long ( )

log2 :: Integer -> Integer
log2 n = if n <= 1 then 0 else succ $ log2 ( n `div` 2 )




