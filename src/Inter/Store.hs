module Inter.Store where

-- $Id$

import Util.Datei
import qualified Posix
import qualified Inter.Param as P
import Maybe

store ::  P.Type -> Maybe Int -> IO String
-- von falschen einsendungen: speichert in "latest.input"
-- d. h. überschreibt immer
-- von richtigen einsendungen: speicher in "$pid.input"
-- d. h. eigentlich kein überschreiben
store p mres = do
    let flag = isJust mres
    pid <- if flag then fmap show $ Posix.getProcessID 
	           else return "latest"
    let d =  Datei { pfad = [ "autotool", "done"
			    , P.aufgabe p, P.version p
			    , P.matrikel p
			    , if flag then "OK" else "NO"
			    ]
                   , name = pid ++ ".input" 
		   }
    schreiben d $ P.input p
    return $ pid
