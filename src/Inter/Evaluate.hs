module Inter.Evaluate where

--   $Id$

import Inter.Types
import Inter.Errmsg
import qualified Inter.Param as P

import qualified  Challenger 

import Reporter.Type
import Size
import Reader
import ToDoc

evaluate :: ( Reader b
	    , Size b
	    , Challenger.Measure p i b
	    , ToDoc b
	    , Challenger.Partial p i b
	    )
 	    => p -> i -> P.Type -> Reporter Int
evaluate p i par =
    case parse (parse_complete reader) "input" $ P.input par of
         Left e -> do
	       inform $ text "Syntaxfehler:"
	       reject $ errmsg (P.input_width par) e $ P.input par
	 Right b  -> do
	       inform $ text "gelesen:" <+> toDoc b
	       inform $ text "partiell korrekt?"
	       Challenger.partial     p i b
	       Challenger.demonstrate p i b
	       inform $ text "total korrekt?"
	       Challenger.total       p i b
               let m = Challenger.measure p i b 
	       inform $ text "Lösung ist korrekt und hat Größe" 
		      <+> toDoc m
	       return $ m


    



