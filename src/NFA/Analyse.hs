-- | vom Automaten zum regulären Ausdruck
--  $Id$
module NFA.Analyse 

( module NFA.Analyse
, module NFA.AI
)

where

import NFA.AI

import Autolib.Exp
import Autolib.Exp.Inter

import Autolib.NFA.Type 
import Autolib.NFA.Eq
import Autolib.NFA.Example
import Autolib.Exp.Example
import Autolib.Exp.Einfach

import Inter.Types

import Autolib.Set
import Autolib.ToDoc
import Autolib.Reporter

import qualified Challenger as C
import Data.Typeable

data Analyse = Analyse deriving ( Eq, Ord, Show, Read, Typeable )


instance C.Partial  Analyse AI Exp
  where
    describe p i =  vcat
	             [     text "Finden Sie einen"
	               <+> text "regulären Ausdruck,"
		     , text "der die Sprache" <+> info ( automat i )
		     , text "über dem Alphabet" <+> toDoc ( alphabet $ automat i )
		     , text "beschreibt."
		     ]

    initial p i   = Autolib.Exp.Example.example (alphabet $ automat i)

    partial p i b = do
        ist_einfach b

    total   p i b = do
	f <- equ ( automat i )
		 ( informed ( toDoc b ) 
		   $ inter (std_sigma (setToList $ alphabet $ automat i)) b  )

	assert f $ text "Stimmen die Sprachen überein?"
        return () 

make :: Make
make = direct Analyse NFA.AI.example


