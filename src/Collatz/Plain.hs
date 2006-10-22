module Collatz.Plain where

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

data Collatz_Plain = Collatz_Plain deriving ( Eq, Ord, Show, Read, Typeable )

instance C.Partial Collatz_Plain Integer P.Parameter where

    report Collatz_Plain x = do
        inform $ vcat
	       [ text "Gesucht sind Länge und maximales Element"
	       , text "der Collatz-Folge mit Startzahl" <+> toDoc x
	       ]

    initial Collatz_Plain x = P.one

    total Collatz_Plain x p = do
	assert ( p == P.compute x )
	       $ text "angegebene Parameter sind korrekt?"

instance C.Measure Collatz_Plain Integer P.Parameter where
    measure _ _ _ = 1

make :: Make
make = direct Collatz_Plain ( 27 :: Integer )

instance Generator Collatz_Plain Config ( Integer, P.Parameter ) where
    generator p conf key = do
        seed $ fromIntegral $ hash $ reverse key
	roll conf

instance Project Collatz_Plain ( Integer, P.Parameter ) Integer where
    project p ( i, _ ) = i

qmake :: Make
qmake = quiz Collatz_Plain rc



