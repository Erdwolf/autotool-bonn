module Turing.Nachfolger 

-- -- $Id$

( nachfolger
, folgekonfigurationen -- brauchen wir nicht?
)

where

import Turing
import Turing.Konfiguration
import Schichten


nachfolger :: TUM y z
	   => Turing y z -> Konfiguration y z -> [ Konfiguration y z ]
nachfolger a k = concat $ map setToList $
    schichten (folgekonfigurationen a   ) k

folgekonfigurationen 
    :: TUM y z
    => Turing y z -> Konfiguration y z 
    -> Set (Konfiguration y z)
folgekonfigurationen m k = mkSet $ do
    (y, z, b) <- setToList $ lookupset (tafel m) ( aktuelles_zeichen k
						 , zustand k )
    let k0 = k { aktuelles_zeichen = y
	       , zustand = z
	       , geschichte = k : geschichte k
	       , schritt = succ $ schritt k
	       }

    let weitere xs = case xs of [] -> []; (y : ys) -> ys
    let erstes  xs = case xs of [] -> leerzeichen m; (y : ys) -> y

    return $ case b of
        O -> k0
	L -> k0 { band_links = weitere $ band_links k0
		, aktuelles_zeichen = erstes $ band_links k0
		, band_rechts = aktuelles_zeichen k0 : band_rechts k0
		}
	R -> k0 { band_rechts = weitere $ band_rechts k0
		, aktuelles_zeichen = erstes $ band_rechts k0
		, band_links = aktuelles_zeichen k0 : band_links k0
		}



