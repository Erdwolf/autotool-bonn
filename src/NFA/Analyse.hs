module NFA.Analyse where

-- vom Automaten zum regulären Ausdruck

--  $Id$

import Exp
import Exp.Inter

import NFA.Type
import NFA.Eq
import Exp.Example
import Exp.Einfach

import Inter.Types

import Sets
import ToDoc
import Reporter

import qualified Challenger as C
import Data.Typeable

data Analyse = Analyse deriving ( Eq, Ord, Show, Read, Typeable )

data AI = AI { name :: String -- abkürzung
	     , automat :: NFA Char Int   
	     , alphabet :: Set Char
	     }
    deriving ( Show, Typeable )

instance C.Partial  Analyse AI Exp
  where
    describe p i =  vcat
	             [     text "Finden Sie einen"
	               <+> text "regulären Ausdruck,"
		     , text "der die Sprache" <+> info ( automat i )
		     , text "über dem Alphabet" <+> toDoc ( alphabet i )
		     , text "beschreibt."
		     ]

    initial p i   = Exp.Example.example (alphabet i)

    partial p i b = do
        ist_einfach b

    total   p i b = do
	f <- equ ( automat i )
		 ( informed ( toDoc b ) 
		   $ inter (std_sigma (setToList $ alphabet i)) b  )

	assert f $ text "Stimmen die Sprachen überein?"
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



