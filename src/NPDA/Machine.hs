{-# LANGUAGE MultiParamTypeClasses #-}
module NPDA.Machine where

-- -- $Id$

import Machine.Class
import Machine.Akzeptieren
import Machine.History

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
    input_reporter m xs = return $ start_konfiguration m xs


instance NPDAC x y z 
	 => Out ( NPDA x y z ) [x] ( Konfiguration x y z ) where
    -- endkonf. lesen (ein einziges arg)
    -- output :: m -> conf -> dat
    
    -- TODO: wouzu ist diese Instanz eigentlich nötig?
    -- NPDA soll doch  nur rechnen und akzeptieren, nichts ausgeben?
    output_reporter m k = return $ eingabe k -- sollte verbraucht sein


instance NPDAC x y z 
	 => Compute ( NPDA x y z ) ( Konfiguration x y z ) where
    depth m k = schritt k
    next m k = folgekonfigurationen m k
    weight m k = fromIntegral 
	       $ length ( keller k ) + length ( eingabe k )

    accepting m k = null ( eingabe k ) 
	&& case akzeptiert m of
	        Leerer_Keller -> null $ keller k
	        Zustand qs    -> zustand k `elementOf` qs


