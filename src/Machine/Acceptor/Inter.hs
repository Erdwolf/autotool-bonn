module Machine.Acceptor.Inter where

-- $Id$

import qualified Machine.Acceptor.Type as A
import Machine.Akzeptieren
import Machine.Class

import qualified Challenger as C

import Inter.Types
import Reporter hiding ( output )
import ToDoc
import Informed

acceptor :: ( Machine m dat conf, ToDoc [dat] )
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
	      inform $ text "welche die Sprache" <+> info num 
	               <+> text "akzeptiert!"
	      return num
	}

{-
instance ( Machine m dat conf , ToDoc [dat] ) 
         => C.Partial A.Acceptor ( A.Type m dat conf ) m where
    initial p i   = A.start i
    partial p i b = A.check i b
    total   p i b = do
        positiv_liste (A.cut i) b $ A.yeah i
        negativ_liste (A.cut i) b $ A.noh  i
        return () -- größe der maschine (hier) ignorieren
-}
