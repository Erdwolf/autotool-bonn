module NFA.Analyse where

-- vom Automaten zum regul�ren Ausdruck

-- $Id$

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


data Analyse = Analyse deriving ( Eq, Ord, Show, Read )

data AI = AI { name :: String -- abk�rzung
	     , automat :: NFA Char Int   
	     , alphabet :: Set Char
	     }
    deriving ( Show )

instance C.Partial  Analyse AI Exp
  where
    describe p i =  vcat
	             [     text "Finden Sie einen"
	               <+> text "regul�ren Ausdruck,"
		     , text "der die Sprache" <+> info ( automat i )
		     , text "�ber dem Alphabet" <+> toDoc ( alphabet i )
		     , text "beschreibt."
		     ]

    initial p i   = Exp.Example.example (alphabet i)

    partial p i b = do
        ist_einfach b

    total   p i b = do
	f <- equ ( automat i )
		 ( informed ( toDoc b ) 
		   $ inter (std_sigma (setToList $ alphabet i)) b  )

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



