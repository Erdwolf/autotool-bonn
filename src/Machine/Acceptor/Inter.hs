module Machine.Acceptor.Inter where

--   $Id$

import qualified Machine.Acceptor.Type as A
import Machine.Akzeptieren
import Machine.Class
import Condition

import qualified Challenger as C

import Inter.Types
import Autolib.Reporter hiding ( output )
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Informed

{-
acceptor :: ( Machine m dat conf )
         => String		-- aufgabe (major)
	 -> String -- version ( minor )
     -> A.Type m dat 
     -> Var A.Acceptor ( A.Type m dat ) m
acceptor auf ver num =
    Var { problem = A.Acceptor
	, aufgabe = auf
	, version = ver
	, key = \ matrikel -> do
	      return matrikel
	, gen = \ matrikel -> return $ do
	      inform $ text "Konstruieren Sie eine Maschine,"
	      inform $ text "welche die Sprache" <+> A.data_info num 
	               <+> text "akzeptiert!"
	      return num
	}
-}

instance ( Condition prop m , Reader m,  Machine m dat conf 
	 , ToDoc [ dat ], ToDoc [prop]  , ToDoc prop
	 , Reader [ dat ], Reader [prop]  , Reader prop
	 ) 
         => C.Partial A.Acceptor ( A.Type m dat prop ) m where
    describe p i  = vcat
	          [ text "Gesucht ist ein" <+> A.machine_desc i 
		  , text "für die Sprache" <+> A.data_desc i
		  , text "mit diesen Eigenschaften" <+> toDoc (A.properties i)
		  ]
    initial p i   = A.start i
    partial p i b = investigate ( A.properties i ) b
    total   p i b = do
        positiv_liste (A.cut i) b $ A.yeah i
        negativ_liste (A.cut i) b $ A.noh  i
        return () -- größe der maschine (hier) ignorieren

