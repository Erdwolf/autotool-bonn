{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances #-}

--  $Id$

{-# LANGUAGE TemplateHaskell #-}
module Graph.MST.Weight where

import Autolib.ToDoc
import Autolib.Reader

-- import Text.XML.HaXml.Haskell2Xml

import Autolib.Graph.Type ( Kante (..) )
import Autolib.FiniteMap ( FiniteMap , lookupWithDefaultFM , listToFM )

import Data.Typeable

import Autolib.Hash ( Hash , hash )

import System.Random ( randomRIO )
import System.IO.Unsafe ( unsafePerformIO )

-------------------------------------------------------------------------------

data Weight = Summe
	    | Produkt 
	    | Delta
	    | Hamming Int
	    | Random Int
	    | Direct (FiniteMap (Kante Int) Int)
	      deriving ( Eq, Typeable )

instance Hash Weight where
    hash Summe = 7
    hash Produkt = 11
    hash Delta = 13
    hash (Hamming n) = 3 * (hash $ succ n)
    hash (Random n) = 5 * (hash $ succ n)
    hash (Direct fm) = 2 * (hash fm)

wfun :: Weight-> Kante Int -> Int
wfun Summe       (Kante {von=v,nach=n}) = v+n
wfun Produkt     (Kante {von=v,nach=n}) = v*n
wfun Delta       (Kante {von=v,nach=n}) = abs $ v-n
wfun (Hamming k) (Kante {von=v,nach=n}) = 
    let digs 0  = []
	digs x  = let (q,r) = divMod x 2 in r : digs q
	digsK x = take k $ digs x ++ repeat 0
    in length $ filter id $ zipWith (/=) (digsK v) (digsK n)
wfun (Random k)  _ = unsafePerformIO $ randomRIO (1,k)
wfun (Direct fm) k = lookupWithDefaultFM fm (error "gewichte nicht komplett!?") k

direct :: Weight -> [Kante Int] -> Weight
direct f ks = Direct $ listToFM $ do k <- ks ; return (k,wfun f k)

$(derives [makeReader, makeToDoc] [''Weight])
-- {-! for Weight derive: Reader, ToDoc, Haskell2Xml !-}

-- Local Variables:
-- mode: haskell
-- End:

