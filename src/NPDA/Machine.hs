module NPDA.Machine where

-- $Id$

import Machine.Class
import Machine.Akzeptieren
import Machine.History

import qualified Challenger as C
import qualified Machine.Acceptor.Type as A

import NPDA.Type
import NPDA.Konfiguration
import NPDA.Nachfolger

instance NPDAC x y z
	 =>  History ( Konfiguration x y z ) where
    history k = tail $ links k

instance NPDAC x y z 
	 => In ( NPDA x y z ) [x] ( Konfiguration x y z ) where
    -- startkonf. herstellen (tupel von args)
    -- input  :: m -> dat -> conf
    input m xs = start_konfiguration m xs


instance NPDAC x y z 
	 => Out ( NPDA x y z ) [x] ( Konfiguration x y z ) where
    -- endkonf. lesen (ein einziges arg)
    -- output :: m -> conf -> dat
    output m k = eingabe k -- sollte verbraucht sein


instance NPDAC x y z 
	 => Compute ( NPDA x y z ) ( Konfiguration x y z ) where
    next m k = folgekonfigurationen m k
    accepting m k = case akzeptiert m of
        Leerer_Keller -> null $ keller k
	Zustand qs    -> zustand k `elementOf` qs

