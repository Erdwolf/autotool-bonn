module Turing.Konfiguration where

-- $Id$

import Turing


data Konfiguration y z = 
     Konfiguration { band_links :: [ y ]
		   , aktuelles_zeichen :: y
		   , band_rechts :: [ y ]
		   , zustand :: z
		   , geschichte :: [Konfiguration y z] 
		   }

nummer :: TUM y z
       => Konfiguration y z -> Int
nummer = length . geschichte

instance TUM y z
	 => Show (Konfiguration y z) where
    showsPrec p k = showString $ 
	           ( show (reverse (band_links k)) )
	 ++ " " ++ ( show (zustand k, aktuelles_zeichen k) )
	 ++ " " ++ ( show (         band_rechts k) )


showlinks :: TUM y z
	 => Konfiguration y z -> String
showlinks = unlines . map show . links



wesentlich k = 
    (band_links k, aktuelles_zeichen k, band_rechts k, zustand k)
    -- aber link nicht

instance (Eq y, Eq z) => Eq (Konfiguration y z) where
   k1 == k2  =  wesentlich k1 == wesentlich k2

instance (Ord y, Ord z) => Ord (Konfiguration y z) where
   k1 `compare` k2  =  wesentlich k1 `compare` wesentlich k2


links :: Konfiguration y z -> [ Konfiguration y z ]
links k = k : geschichte k


start_konfiguration :: TUM y z
		    => Turing y z -> [y] -> Konfiguration y z
start_konfiguration m xs = 
    Konfiguration { band_links = []
		  , aktuelles_zeichen = case xs of [] -> leerzeichen m
						   (y : ys) -> y
		  , zustand = startzustand m
		  , band_rechts =  case xs of [] -> []
					      (y : ys) -> ys
		  , geschichte = []
		  }




