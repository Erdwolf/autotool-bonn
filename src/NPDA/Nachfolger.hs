module Autolib.NPDA.Nachfolger 

--   $Id$

( nachfolger
, folgekonfigurationen -- brauchen wir nicht?
)

where

import Autolib.NPDA.Type
import Autolib.NPDA.Konfiguration

import Autolib.Schichten
import Autolib.Set



nachfolger :: NPDAC x y z
	   => NPDA x y z -> Konfiguration x y z -> [ Konfiguration x y z ]
nachfolger a k = concat $ map setToList $
    schichten (folgekonfigurationen a   ) k

sinnvoll ::  NPDAC x y z
	 => Konfiguration x y z -> Bool
sinnvoll k =
    length (keller k) <= 2 * length (eingabe k) + 3


folgekonfigurationen 
    :: NPDAC x y z
    => NPDA x y z -> Konfiguration x y z 
    -> Set (Konfiguration x y z)
folgekonfigurationen a k 
    = mkSet $ filter sinnvoll $ setToList
    $ epsilon_schritte a k `union` einer_schritte a k

epsilon_schritte a k = 
    case keller k of
        [] -> emptySet -- Keller leer, nichts geht mehr
	(y : ys) -> mkSet $ do
	   (z', y') <- setToList $ lookupset (tafel a) (Nothing, zustand k, y)
	   return $ Konfiguration { eingabe = eingabe k
				  , zustand = z'
				  , keller  = y' ++ ys
				  , link    = Just k
				  }

einer_schritte a k = case keller k of
	 [] -> emptySet 
	 (y : ys) -> case eingabe k of
	  [] -> emptySet -- Eingabe leer, nichts geht mehr
	  (x : xs) -> mkSet $ do
	    (z', y') <- setToList $ lookupset (tafel a) (Just x, zustand k, y)
	    return $ Konfiguration { eingabe = xs
				   , zustand = z'
				   , keller  = y' ++ ys
				   , link    = Just k
				   }


