module Machine.Fun.Inter where

--   $Id$

import qualified Machine.Fun.Type as F
import Machine.Fun
import Machine.Class

import qualified Challenger as C

import Inter.Types
import Reporter hiding ( output )
import ToDoc
import Informed

computer :: ( Machine m dat conf, InOut m dat conf )
         => String		-- aufgabe (major)
	 -> String -- version ( minor )
     -> F.Type m dat
     -> Var F.Computer ( F.Type m dat ) m 
computer auf ver num = computer_mat auf ver ( const num )

computer_mat :: ( Machine m dat conf, InOut m dat conf )
         => String		-- aufgabe (major)
	 -> String -- version ( minor )
     -> ( Key -> F.Type m dat )
     -> Var F.Computer ( F.Type m dat ) m 
computer_mat auf ver fnum =
    Var { problem = F.Computer
	, aufgabe = auf
	, version = ver
	, key = \ matrikel -> do
	      return matrikel
	, gen = \ key -> return $ do
	      return $ fnum key
	}

instance ( Machine m dat conf, InOut m dat conf )
         => C.Partial F.Computer (F.Type m dat) m where
    describe p i =
	    vcat [ text "Konstruieren Sie eine Maschine,"
		 , text "welche die Funktion" <+> info i
	               <+> text "berechnet!"
		 , text "Einige (Argument,Wert)-Paare dieser Funktion sind:"
		 , nest 4 $ toDoc $ take 5 $ F.pairs i
		 ]

    initial p i   = F.start i
    partial p i b = F.check i b
    total   p i b = do
        fun_test (F.cut i) (F.pairs i) b
        return () -- größe der maschine (hier) ignorieren


