module Turing.Machine where

-- $Id$

import Machine.Class

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



