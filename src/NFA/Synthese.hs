module NFA.Synthese 

( module NFA.Synthese
, module NFA.SI
)

where

-- vom regulären Ausdruck zum Automaten

-- -- $Id$

import NFA.SI

import Autolib.Exp
import Autolib.Exp.Inter

import Autolib.NFA.Type hiding ( alphabet )
import Autolib.NFA.Eq
import qualified Autolib.NFA.Example
import qualified Autolib.NFA.Check
import Autolib.NFA.Restrict

import Inter.Types

import Autolib.Set
import Autolib.ToDoc
import Autolib.Reporter

import qualified Challenger as C
import Data.Typeable

data Synthese = Synthese deriving ( Eq, Ord, Show, Read, Typeable )


besch :: SI -> Doc
besch i = case beschreibung i 
			      of Just d -> text d
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

    initial p i   = Autolib.NFA.Example.example_sigma $ alphabet i

    partial p i b = do
        restrict_states b
	restrict_alpha ( alphabet i ) b

    total   p i b = do
        let goal = inter (std_sigma $ setToList $ alphabet i) (ausdruck i)
	f <- equ ( informed ( besch i )                         goal )
		 ( informed ( text "Sprache Ihres Automaten" )  b    )

	assert f $ text "Stimmen die Sprachen überein?"
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

make :: Make
make = direct Synthese NFA.SI.example


