module NPDA.Info where

-- -- $Id$

import NPDA.Type
import NPDA.Konfiguration

import NPDA.Vorganger

import Monad (guard)

mit_leerem_keller_akzeptierte_eingaben
    :: (Ord x, Ord y, Ord z)
    => NPDA x y z -> [ Konfiguration x y z ]
mit_leerem_keller_akzeptierte_eingaben a = take 10 $ do
    let ks = leere_keller_konfigurationen a
    k <- take 1000 $ vorganger a ks
    guard $ keller k == [ startsymbol a ]
    guard $ zustand k == startzustand a
    return k

