module Inter.Store where

--  $Id$

import Util.Datei
import qualified System.Posix
import qualified Inter.Param as P

import Control.Types (toString)

import Data.Maybe

store ::  P.Type -> Maybe Int -> IO String
-- von falschen einsendungen: speichert in "latest.input"
-- d. h. �berschreibt immer
-- von richtigen einsendungen: speicher in "$pid.input"
-- d. h. eigentlich kein �berschreiben
store p mres = do
    let flag = isJust mres
    pid <- if flag then fmap show $ System.Posix.getProcessID 
	           else return "latest"
    schreiben ( location p pid flag ) $ P.input p
    return $ pid

latest :: P.Type -> IO String
latest p = do
    lesen ( location p "latest" False ) 

load :: P.Type -> String -> Bool -> IO String
load p pid flag = do
    lesen ( location p pid flag )
    

location :: P.Type -> String -> Bool -> Datei
location p pid flag =  
    Datei { pfad = [ "autotool", "done"
			    , toString $ P.vnr p
		            , toString $ P.anr p
			    , P.smatrikel p
			    , if flag then "OK" else "NO"
			    ]
                   , name = pid
		   , extension = "input" 
		   } 