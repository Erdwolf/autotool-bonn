module NFA.Synthese where

-- vom regulären Ausdruck zum Automaten

-- $Id$

import Exp
import Exp.Inter

import NFA.Type
import NFA.Eq
import qualified NFA.Example
import qualified NFA.Check
import NFA.Restrict

import Inter.Types

import Sets
import ToDoc
import Reporter

import qualified Challenger as C


data Synthese = Synthese deriving ( Eq, Ord, Show, Read )

data SI = SI { name :: String -- abkürzung
	     , ausdruck :: Exp   
	     , alphabet :: Set Char
	     , beschreibung :: Maybe Doc -- falls Nothing, dann ausdruck
	     , deterministisch :: Bool      -- soll deterministisch sein?
	     }
    deriving ( Show )

besch :: SI -> Doc
besch i = case beschreibung i 
			      of Just d -> d
			         Nothing -> toDoc $ ausdruck i


instance C.Partial  Synthese SI ( NFA Char Int )
  where
    describe p i = vcat
        [ text "Finden Sie einen"
                       <+> ( if deterministisch i 
			     then text "deterministischen" else empty )
	               <+> text "endlichen Automaten,"
	, text "der die Sprache" <+> besch i
	, text "über dem Alphabet" <+> toDoc ( alphabet i )
	, text "akzeptiert."
	]

    initial p i   = NFA.Example.example

    partial p i b = do
        restrict_states b
	restrict_alpha ( alphabet i ) b

    total   p i b = do
        let goal = inter (std_sigma $ setToList $ alphabet i) (ausdruck i)
	f <- equ ( informed ( besch i )                         goal )
		 ( informed ( text "Sprache Ihres Automaten" )  b    )

	assert f $ text "Stimmen die Sprachen überein?"
        when (  deterministisch i ) $ NFA.Check.deterministisch b
        return () 

synthese :: String -- aufgabe (major)
	 -> String -- aufgabe (minor)
	 -> SI
	 -> Var  Synthese SI ( NFA Char Int )
synthese auf ver i = 
    Var { problem = Synthese
	, aufgabe = auf
	, version = ver
	, key = \ matrikel -> do 
	      return ""
	, gen = \ key -> return $ do
	      return i
	}



