module Inter.Evaluate where

--   $Id$

import Inter.Area

import Inter.Types
import Inter.Errmsg
import qualified Inter.Param as P

import Control.Types ( Wert (..) )

import qualified  Challenger 

import Autolib.Reporter.Type
import Autolib.Reader
import Autolib.ToDoc

evaluate :: ( Reader b
	    , ToDoc b
	    , Challenger.Partial p i b
	    )
 	    => p -> i -> String 
	    -> Reporter Wert
evaluate p i cs =
    case parse (parse_complete reader) "input" cs of
         Left e -> do
	       inform $ text "Syntaxfehler:"
	       reject $ errmsg 72 e $ cs
	 Right b  -> do
	       inform $ text "gelesen:" <+> toDoc b
               evaluate' p i b

evaluate' p i b = do
       inform $ text "partiell korrekt?"
       Challenger.partial     p i b
       Challenger.demonstrate p i b
       inform $ text "total korrekt?"
       code <- Challenger.total_neu       p i b
       -- let m = Challenger.measure p i b 
       inform $ text "Bewertung der Einsendung:" <+> toDoc code
       return $ code


    



