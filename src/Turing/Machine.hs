module Turing.Machine where

--   $Id$

import Machine.Class
import Machine.Akzeptieren

import qualified Challenger as C
import qualified Machine.Acceptor.Type as A

import Turing.Type
import Turing.Konfiguration
import Turing.Nachfolger ( folgekonfigurationen )

import qualified Reporter.Checker
import Data.Set
import Size
import ToDoc

instance TUM y z => Compute ( Turing y z ) ( Konfiguration y z ) where
    next m k = folgekonfigurationen m k
    accepting m k = zustand k `elementOf` endzustandsmenge m

instance TUM y z => In  ( Turing y z ) [ y ] ( Konfiguration y z ) where
    input  m ys = start_konfiguration m ys

instance TUM y z => Out  ( Turing y z ) [ y ] ( Konfiguration y z ) where
    output m k = bandinhalt m k

instance Numerical String where
    -- un�r
    encode xs = do
        x <- xs
	replicate ( fromIntegral x) '1' ++ ","  -- nicht das leerzeichen!
    decode m = fromIntegral $ length m -- eigentlich pr�fen, welche zeichen


instance TUM y z => 
        C.Partial A.Acceptor ( A.Type ( Turing y z ) [y] ) 
	    ( Turing y z ) 
  where
    describe p i  = vcat
        [ text "Gesucht ist eine Maschine/ein Programm,"
	, nest 4 $ A.machine_info i
	, text "das diese Sprache akzeptiert:"
	, nest 4 $ A.data_info i
	, text "diese Eingaben sollen akzeptiert werden:"
	, nest 4 $ toDoc $ A.yeah i
	, text "diese Eingaben sollen nicht werden:"
	, nest 4 $ toDoc $ A.noh i
	]
    initial p i   = A.start i
    partial p i b = Reporter.Checker.run ( A.check i ) b
    total   p i b = do
        positiv_liste (A.cut i) b $ A.yeah i
        negativ_liste (A.cut i) b $ A.noh  i
        return () -- gr��e der maschine (hier) ignorieren



