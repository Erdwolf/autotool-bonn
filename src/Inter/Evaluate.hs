module Inter.Evaluate where

--   $Id$

import Inter.Area

import Inter.Types
import Inter.Errmsg
import qualified Inter.Param as P

import qualified  Challenger 

import Autolib.Reporter.Type
import Autolib.Size
import Autolib.Reader
import Autolib.ToDoc

evaluate :: ( Reader b
	    , Size b
	    , ToDoc b
	    , Challenger.Partial p i b
	    )
 	    => p -> i -> String -> Reporter Int
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
       Challenger.total       p i b
       let m = Challenger.measure p i b 
       inform $ text "Lösung ist korrekt und hat Größe" 
	      <+> toDoc m
       return $ m


    



