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



instance C.Partial Collatz_Long Int (Integer, P.Parameter) where

    report Collatz_Long top = do
        inform $ vcat
	       [ text "Gesucht ist eine lange Collatz-Folge."
               , text "Geben Sie ein Paar (Startzahl, Parameter) ein."
               , text "Bewertet wird  Parameter.length - log_2(Startzahl)."
               , text "Die Startzahl soll höchstens" <+> toDoc top 
                          <+> text "Dezimalstellen haben." 
	       ]

    initial Collatz_Long top = ( 7, P.compute 5 )

    total Collatz_Long top ( start, p ) = do
        assert ( length ( show start ) <= top )
               $ text "Startzahl nicht zu groß?"
	assert ( P.compute start == p )
	       $ text "angegebene Parameter sind korrekt?"

instance C.Measure Collatz_Long Int (Integer, P.Parameter) where
    measure Collatz_Long top ( start, p ) = 
        max 0 $ P.length p - log2 start 

make :: Make
make = direct Collatz_Long ( 1000 :: Int )

log2 :: Integer -> Integer
log2 n = if n <= 1 then 0 else succ $ log2 ( n `div` 2 )




