module Inter.Bank where

-- -- $Id$

import Control.Punkt
import Control.Types
import qualified System.Posix

import System.Time
import qualified Inter.Param as P
import qualified  Autolib.Reporter.Result

import qualified Inter.Store
import Util.Datei

bank :: P.Type -> Maybe Int -> IO String
bank p res = do
    let it = case res of Just s -> Ok s ; Nothing -> No
    bepunkteStudentDB (P.ident p) (P.anr p) it (P.highscore p)

    pid <- Inter.Store.store p res

    time <- zeit
    let msg = logline  time pid p res


    d <- datum
    let logcgi = Datei	{ pfad = [ "autotool", "log" ] ++ d
			, name = "CGI"
			, extension = ""
			}
    anhaengen logcgi msg

    return msg


logline time pid p res = unwords [ time
		      , "(",  pid, ")"
		      , "cgi-" ++ P.smatrikel p
		     , "(", P.smatrikel p, ")"
		      , P.subject p , ":"
		     , Autolib.Reporter.Result.result_string res
		     , "\n"
		     ]


datum :: IO [ String ]
datum = do
    clock <- getClockTime
    cal <- toCalendarTime clock    
    return [ show $ ctYear cal
	   , show $ ctMonth cal
	   , show $ ctDay cal
	   ]

zeit :: IO String
zeit = do
    clock <- getClockTime
    cal <- toCalendarTime clock    
    return $ calendarTimeToString cal

