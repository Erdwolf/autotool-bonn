module Machine.Numerical.Inter where

-- -- $Id$

import qualified Machine.Numerical.Type as N
import Machine.Fun
import Machine.Class

import qualified Challenger as C

import Inter.Types
import Reporter hiding ( output )
import ToDoc
import Informed

computer :: ( Machine m dat conf, Numerical dat )
         => String		-- aufgabe (major)
	 -> String -- version ( minor )
     -> N.Type m
     -> Var N.Computer ( N.Type m ) m 
computer auf ver num = computer_mat auf ver ( const num )

computer_mat :: ( Machine m dat conf, Numerical dat )
         => String		-- aufgabe (major)
	 -> String -- version ( minor )
     -> ( Key -> N.Type m )
     -> Var N.Computer ( N.Type m ) m 
computer_mat auf ver fnum =
    Var { problem = N.Computer
	, aufgabe = auf
	, version = ver
	, key = \ matrikel -> do
	      return matrikel
	, gen = \ key -> return $ do
	      return $ fnum key
	}

instance ( Machine m dat conf, Numerical dat )
         => C.Partial N.Computer (N.Type m) m where
    describe p i =
	    vcat [ text "Konstruieren Sie eine Maschine,"
		 , text "welche die Funktion" <+> info i
	               <+> text "berechnet!"
		 ]

    initial p i   = N.start i
    partial p i b = N.check i b
    total   p i b = do
        numerical_test' i b
        return () -- größe der maschine (hier) ignorieren

