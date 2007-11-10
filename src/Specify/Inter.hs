{-# OPTIONS -fglasgow-exts #-}

module Specify.Inter where

import Specify.Config
import Specify.Definition
import Specify.Check

import Autolib.Reporter
import Autolib.ToDoc

import qualified Challenger as C
import Inter.Types
import Data.Typeable

data Specify = Specify deriving ( Eq, Ord, Show, Read, Typeable )

instance C.Partial Specify Config Program where
    describe s c = vcat
        [ text "Deklarieren Sie Funktionen mit den folgenden Eigenschaften,"
	, text "wobei die Variablen über alle natürlichen Zahlen laufen:"
	, nest 4 $ toDoc $ constraints c
	]
    initial s c = Specify.Definition.example
    total s c p = do
        Specify.Check.full ( constraints c ) p ( checks_per_constraint c )

make :: Make
make = direct Specify Specify.Config.example

