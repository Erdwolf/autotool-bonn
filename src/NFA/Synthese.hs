module NFA.Synthese where

-- vom regul�ren Ausdruck zum Automaten

-- -- $Id$

import Autolib.Exp
import Autolib.Exp.Inter

import Autolib.NFA.Type
import Autolib.NFA.Eq
import qualified Autolib.NFA.Example
import qualified Autolib.NFA.Check
import Autolib.NFA.Restrict

import Inter.Types

import Autolib.Sets
import Autolib.ToDoc
import Autolib.Reporter

import qualified Challenger as C


data Synthese = Synthese deriving ( Eq, Ord, Show, Read )

data SI = SI { name :: String -- abk�rzung
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
	, text "�ber dem Alphabet" <+> toDoc ( alphabet i )
	, text "akzeptiert."
	]

    initial p i   = Autolib.NFA.Example.example_sigma $ alphabet i

    partial p i b = do
        restrict_states b
	restrict_alpha ( alphabet i ) b

    total   p i b = do
        let goal = inter (std_sigma $ setToList $ alphabet i) (ausdruck i)
	f <- equ ( informed ( besch i )                         goal )
		 ( informed ( text "Sprache Ihres Automaten" )  b    )

	assert f $ text "Stimmen die Sprachen �berein?"
        when (  deterministisch i ) $ Autolib.NFA.Check.deterministisch b
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



