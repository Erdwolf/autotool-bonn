module NFA.Synthese where

-- vom regulären Ausdruck zum Automaten

-- $Id$

import Exp
import Exp.Inter

import NFA.Type
import NFA.Eq
import qualified NFA.Example

import Inter.Types

import Sets
import ToDoc
import Reporter

import qualified Challenger as C


data Synthese = Synthese deriving ( Eq, Ord, Show, Read )

instance C.Partial  Synthese ( Exp, Set Char ) ( NFA Char Int )
  where
    initial p i   = NFA.Example.example
    partial p i b = return ()
    total   p (e, a) b = do
        let goal = inter (std_sigma $ setToList a) e
	f <- equ goal b
	assert f $ text "Stimmen die Sprachen überein?"
        return () 

synthese :: String -- aufgabe (major)
	 -> String -- aufgabe (minor)
	 -> ( Exp, Set Char)    -- ausdruck, alphabet
	 -> Var  Synthese ( Exp, Set Char ) ( NFA Char Int )
synthese auf ver ( e, a ) = 
    Var { problem = Synthese
	, aufgabe = auf
	, version = ver
	, key = \ matrikel -> do 
	      return ""
	, gen = \ key -> return $ do
	      inform $ vcat
	             [ text "Finden Sie einen endlichen Automaten,"
		     , text "der die Sprache" <+> toDoc e
		     , text "über dem Alphabet" <+> toDoc a
		     , text "akzeptiert."
		     ]
	      return ( e, a )
	}



