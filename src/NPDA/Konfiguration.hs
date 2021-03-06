-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module NPDA.Konfiguration where

--   $Id$

import NPDA.Type

import Autolib.ToDoc
import Autolib.Set

import Data.Typeable

data Konfiguration x y z = 
     Konfiguration { schritt :: Int
		   , eingabe :: [ x ]
		   , zustand :: z
		   , keller  :: [ y ]
		   , link :: Maybe (Konfiguration x y z)
		   }
    deriving ( Typeable )

instance NPDAC x y z => ToDoc (Konfiguration x y z) where
    toDoc k = text "Konfiguration" <+> dutch_record
        [ text "schritt" <+> equals <+> toDoc (schritt k) 
	, text "eingabe" <+> equals <+> toDoc (eingabe k) 
	, text "zustand" <+> equals <+> toDoc (zustand k) 
	, text "keller"  <+> equals <+> toDoc (keller  k) 
	] 

-- keine Reader/Read-Instanzen

doclinks :: NPDAC x y z
	 => Konfiguration x y z -> Doc
doclinks = fsep . map toDoc . links

---------------------------------------------------------------------------

wesentlich k = (eingabe k, zustand k, keller k) -- aber link nicht

instance (Eq x, Eq y, Eq z) => Eq (Konfiguration x y z) where
   k1 == k2  =  wesentlich k1 == wesentlich k2

instance (Ord x, Ord y, Ord z) => Ord (Konfiguration x y z) where
   k1 `compare` k2  =  wesentlich k1 `compare` wesentlich k2

links :: Konfiguration x y z -> [ Konfiguration x y z ]
links k = k : case link k of
    Just k' -> links k'; Nothing -> []

start_konfiguration :: NPDAC x y z 
		    => NPDA x y z -> [x] -> Konfiguration x y z
start_konfiguration a xs = 
    Konfiguration { schritt = 0
		  , eingabe = xs
		  , zustand = startzustand a
		  , keller = [ startsymbol a ]
		  , link = Nothing
		  }

leere_keller_konfigurationen :: NPDAC x y z 
	     => NPDA x y z -> Set (Konfiguration x y z)
leere_keller_konfigurationen a = mkSet $ do
    z <- setToList $ zustandsmenge a
    return $ Konfiguration { schritt = 0 -- ??
			   , eingabe = []
			   , keller = []
			   , zustand = z
			   , link = Nothing
			   }
