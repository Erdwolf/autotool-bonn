-- | vom Automaten zum regul�ren Ausdruck

module NFA.Analyse 

( module NFA.Analyse
, module NFA.AI
)

where


--  $Id$

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
	               <+> text "regul�ren Ausdruck,"
		     , text "der die Sprache" <+> info ( automat i )
		     , text "�ber dem Alphabet" <+> toDoc ( alphabet $ automat i )
		     , text "beschreibt."
		     ]

    initial p i   = Autolib.Exp.Example.example (alphabet $ automat i)

    partial p i b = do
        ist_einfach b

    total   p i b = do
	f <- equ ( automat i )
		 ( informed ( toDoc b ) 
		   $ inter (std_sigma (setToList $ alphabet $ automat i)) b  )

	assert f $ text "Stimmen die Sprachen �berein?"
        return () 

analyse :: String -- aufgabe (major)
	 -> String -- aufgabe (minor)
	 -> AI
	 -> Var  Analyse AI Exp
analyse auf ver i = 
    Var { problem = Analyse
	, aufgabe = auf
	, version = ver
	, key = \ matrikel -> do 
	      return ""
	, gen = \ key -> return $ do
	      return i
	}

make :: Make
make = direct Analyse NFA.AI.example


