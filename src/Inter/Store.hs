module Inter.Store where

--  $Id$

import Util.Datei
import qualified System.Posix
import qualified Inter.Param as P

import Control.Types (toString, fromCGI, File)
import Control.Monad ( when )
import Inter.Logged

import Data.Maybe

-- | alles: speichert in "latest.input"
-- d. h. überschreibt immer
-- zur sicherheit auch: von richtigen einsendungen: speicher in "$pid.input"
-- d. h. eigentlich kein überschreiben
store ::  P.Type -> Maybe Integer -> IO ( String, File )
store p mres = logged "Inter.store" $ do
    pid <- fmap show $ System.Posix.getProcessID 
    let flag = isJust mres
    when flag $ do
        schreiben ( location p pid flag ) $ P.input p
        return ()
    f <- schreiben ( location p "latest" flag ) $ P.input p
    return ( pid , fromCGI f )

latest :: P.Type -> IO String
latest p = logged "Inter.latest" $ do
    lesen ( location p "latest" False ) 

load :: P.Type -> String -> Bool -> IO String
load p pid flag = logged "Inter.load" $ do
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