module Machine.Clock.Inter where

--   $Id$

import qualified Machine.Clock.Type as C
import Machine.Clock.Test
import Machine.Fun
import Machine.Class

import qualified Challenger

import Inter.Types
import Reporter hiding ( output )
import ToDoc
import Informed

clock :: ( Machine m dat conf, Encode dat )
         => String		-- aufgabe (major)
	 -> String -- version ( minor )
     -> C.Type m
     -> Var C.Clock ( C.Type m ) m 
clock auf ver num = clock_mat auf ver ( const num )

clock_mat :: ( Machine m dat conf, Encode dat )
         => String		-- aufgabe (major)
	 -> String -- version ( minor )
     -> ( Key -> C.Type m )
     -> Var C.Clock ( C.Type m ) m 
clock_mat auf ver fnum =
    Var { problem = C.Clock
	, aufgabe = auf
	, version = ver
	, key = \ matrikel -> do
	      return matrikel
	, gen = \ key -> return $ do
	      return $ fnum key
	}

instance ( Machine m dat conf, Encode dat )
         => Challenger.Partial C.Clock (C.Type m) m where
    describe p i =
	    vcat [ text "Konstruieren Sie eine Maschine"
		 , text "mit Laufzeit-Funktion" <+> info i
		 , text "Einige (Argument,Wert)-Paare sind:"
		 , nest 4 $ toDoc $ do 
		       n <- C.args i
		       return ( n, C.fun i n )
		 ]

    initial p i   = C.start i
    partial p i b = C.check i b
    total   p i b = do
        clock_test i b
        return () -- größe der maschine (hier) ignorieren

