{-# OPTIONS -fallow-overlapping-instances -fglasgow-exts #-}

module PL.Interpretation where

import PL.Struktur
import PL.Signatur

import Autolib.Set
import Autolib.Size
import Autolib.FiniteMap
import Autolib.TES.Identifier
import Autolib.Reader
import Autolib.ToDoc
import Autolib.Reporter
import qualified Autolib.Reporter.Set as S

import Data.Typeable

data Ord u => Interpretation u =
     Interpretation { struktur :: Struktur u
		    , belegung :: FiniteMap Identifier u
		    }
     deriving ( Typeable )

instance Ord u => Size ( Interpretation u ) where
    size i = size ( struktur i ) + sizeFM ( belegung i )

instance ( Ord u, ToDoc u ) => Signed ( Interpretation u ) where
    check sig i = do
        check sig $ struktur i
	check_variablen ( freie_variablen sig ) ( belegung i )

check_variablen fvs bel = do
    silent $ S.eq ( text "Variablenmenge aus Signatur", fvs )
	    ( text "Variablen aus Belegung", mkSet $ keysFM bel )

empty :: Ord u
      => Signatur
      -> Set u 
      -> Interpretation u
empty sig uni = Interpretation
	  { struktur = PL.Struktur.empty sig uni
	  , belegung = leere_belegung sig uni
	  }

leere_belegung sig uni = listToFM 
		     $ zip ( setToList $ freie_variablen sig )
		     $ concat $ repeat $ setToList uni

{-! for Interpretation derive: Reader, ToDoc !-}

-- local variables:
-- mode: haskell
-- end:
