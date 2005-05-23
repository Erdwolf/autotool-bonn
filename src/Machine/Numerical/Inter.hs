module Machine.Numerical.Inter where

--   $Id$

import qualified Machine.Numerical.Type as N
import Machine.Fun
import Machine.Class

import qualified Challenger as C

import Inter.Types
import Autolib.Reporter hiding ( output )
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Informed

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
	, tag = auf ++ "-" ++ ver
	, key = \ matrikel -> do
	      return matrikel
	, gen = \ vnr manr key -> return $ do
	      return $ fnum key
	}

instance ( Machine m dat conf, Numerical dat, Reader m )
         => C.Partial N.Computer (N.Type m) m where
    describe p i =
	    vcat [ text "Konstruieren Sie eine Maschine,"
		 , text "die die Funktion" <+> info i
	           <+> text "berechnet!"
		 , N.extra_info i
		 ]

    initial p i   = N.start i
    partial p i b = N.check i b
    total   p i b = do
        numerical_test' i b
        return () -- größe der maschine (hier) ignorieren

