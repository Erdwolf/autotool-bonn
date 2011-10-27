module Inter.Evaluate where

--   $Id$

import Inter.Types
import Gateway.Errmsg

import Control.Types ( Wert (..) )

import qualified  Challenger 

import Autolib.Reporter.IO.Type
import Autolib.Reader
import Autolib.ToDoc

evaluate :: ( Reader b
	    , ToDoc b
	    , Challenger.Partial p i b
	    )
 	    => p -> i -> String 
	    -> Reporter Wert
evaluate p i cs = do
    b <- parse_or_complain cs
    evaluate' p i b

{-
    case parse (parse_complete reader) "input" cs of
         Left e -> do
	       inform $ text "Syntaxfehler:"
	       reject $ errmsg 72 e $ cs
	 Right b  -> do
	       inform $ text "gelesen:" <+> toDoc b
               evaluate' p i b
-}

parse_or_complain :: ( Reader a, ToDoc a ) 
                  => String -> Reporter a
parse_or_complain cs = 
    case parse (parse_complete reader) "input" cs of
         Left e -> do
	       inform $ text "Syntaxfehler:"
	       reject $ errmsg 72 e $ cs
	 Right b  -> do
	       inform $ text "gelesen:" <+> toDoc b
               return b

evaluate' p i b = do
       inform $ text "Einsendung korrekt?"
       --inform $ text "partiell korrekt?"
       lift $ Challenger.partial     p i b
       lift $ Challenger.demonstrate p i b
       --inform $ text "total korrekt?"
       code <- Challenger.total_neu       p i b
       --inform $ text "Bewertung der Einsendung:" <+> toDoc code
       return $ code


    



