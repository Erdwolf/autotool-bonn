module Turing.Machine where

-- -- $Id$

import Machine.Class
import Machine.Akzeptieren
import qualified Challenger as C
import qualified Machine.Acceptor.Type as A

import Turing.Type
import Turing.Konfiguration
import Turing.Nachfolger ( folgekonfigurationen )

import Set
import Size


instance TUM y z => Compute ( Turing y z ) ( Konfiguration y z ) where
    next m k = folgekonfigurationen m k
    accepting m k = zustand k `elementOf` endzustandsmenge m

instance TUM y z => InOut  ( Turing y z ) [ y ] ( Konfiguration y z ) where
    input  m ys = start_konfiguration m ys
    output m k = bandinhalt m k

instance Numerical String where
    -- unär
    encode xs = do
        x <- xs
	replicate ( fromIntegral x) '1' ++ ","  -- nicht das leerzeichen!
    decode m = fromIntegral $ length m -- eigentlich prüfen, welche zeichen


instance TUM y z => 
        C.Partial A.Acceptor ( A.Type ( Turing y z ) [y] ) 
	    ( Turing y z ) 
  where
    initial p i   = A.start i
    partial p i b = A.check i b
    total   p i b = do
        positiv_liste (A.cut i) b $ A.yeah i
        negativ_liste (A.cut i) b $ A.noh  i
        return () -- größe der maschine (hier) ignorieren



