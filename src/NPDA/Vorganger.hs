module NPDA.Vorganger 

--   $Id$

( vorganger
, vorganger_konfigurationen -- brauchen wir nicht?
)

where

import NPDA.Type
import NPDA.Konfiguration

import Control.Monad (guard)
import Autolib.Schichten


vorganger :: NPDAC x y z
	   => NPDA x y z -> Set (Konfiguration x y z) 
	   -> [ Konfiguration x y z ]
vorganger a ks = concat $ map setToList $
    schichten' (vorganger_konfigurationen a   ) ks



vorganger_konfigurationen 
    :: NPDAC x y z
    => NPDA x y z -> Konfiguration x y z 
    -> Set (Konfiguration x y z)

vorganger_konfigurationen a k = mkSet $ do
    ((mx, z, y), zys) <- fmToList (tafel a)
    (z', y') <- setToList zys
    let xs = case mx of Just x -> [x]; Nothing -> []
    guard $ z' == zustand k
    let (y1, y2) = splitAt (length y') (keller k)
    guard $ y1 == y'
    return $ Konfiguration { eingabe = xs  ++ eingabe k
			   , keller  = [y] ++ y2
			   , zustand = z
			   , link    = Just k
			   }
    
    


