module Inter.Evaluate where

-- $Id$

import Inter.Types
import Inter.Errmsg
import qualified Inter.Param as P

import qualified  Challenger 

import Reporter.Type
import Size
import Reader
import ToDoc

evaluate :: ( Reader b, Size b
	    , Challenger.Problem p i b
	    , Challenger.Partial p i b
	    )
 	    => p -> i -> P.Type -> Reporter Int
evaluate p i par =
    case parse reader "input" $ P.input par of
         Left e -> reject $ errmsg (P.input_width par) e $ P.input par
	 Right b -> do
	       inform $ text "gelesen:" <+> toDoc b
	       inform $ text "partiell korrekt?"
	       pa <- Challenger.partial p i b
	       inform $ text "total korrekt?"
	       to <- Challenger.total p i b
	       return $ size b


    



