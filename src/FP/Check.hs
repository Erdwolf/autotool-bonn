{-# OPTIONS -fglasgow-exts #-}

module FP.Check where

import FP.Env
import FP.Type
import FP.Expression
import FP.Reconstruct
import FP.Instance

import Autolib.Reporter.Type
import Autolib.ToDoc
import Autolib.Size

import Autolib.TES.Term
import Autolib.TES.Position
import Autolib.TES.Identifier

import Data.Typeable
import Inter.Types
import qualified Challenger as C

data FPTypeCheck = FPTypeCheck deriving  ( Eq, Ord, Show, Read, Typeable )

instance C.Partial FPTypeCheck TI ( Expression Identifier ) where

    describe p i = vcat
        [ text "gesucht ist ein Ausdruck vom Typ"
	, nest 4 $ toDoc ( target i )
	, text "(oder einem allgemeineren Typ)"
	, text "in der Signatur"
	, nest 4 $ toDoc ( signature i )
	]

    initial p i = read "map id"

    total p i b = do
	t <- reconstruct ( signature i ) b
	sub <- isinstanceof ( core $ target i ) ( core t )
	return ()

instance C.Measure FPTypeCheck TI ( Expression Identifier ) where
     measure p i b = fromIntegral $ size b

make :: Make
make = direct FPTypeCheck $ FP.Instance.example

