obsolete module NPDA.Akzeptieren 

-- $Id$

where

import NPDA.Type
import NPDA.Konfiguration
import NPDA.Nachfolger
import NPDA.Vorganger


import Reporter
import ToDoc
import Monad (guard)


akzeptierend :: (Ord x, Ord y, Ord z)
    => Int -> NPDA x y z -> [x] -> [ Konfiguration x y z ]
akzeptierend cut a xs = do
    k <- take cut $ nachfolger a (start_konfiguration a xs)
    guard $ eingabe k == []
    guard $ case akzeptiert a of
	  Leerer_Keller -> keller k == []
	  Zustand zs -> zustand k `elementOf` zs
    return k

----------------------------------------------------------------------


mit_leerem_keller_akzeptierte_eingaben
    :: (Ord x, Ord y, Ord z)
    => NPDA x y z -> [ Konfiguration x y z ]
mit_leerem_keller_akzeptierte_eingaben a = take 10 $ do
    let ks = leere_keller_konfigurationen a
    k <- take 1000 $ vorganger a ks
    guard $ keller k == [ startsymbol a ]
    guard $ zustand k == startzustand a
    return k


----------------------------------------------------------------------

