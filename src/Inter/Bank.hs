module Inter.Bank where

-- $Id$

import SQLqueries
import qualified Posix

import Time
import qualified Inter.Param as P
import qualified Reporter.Result

import qualified Inter.Store
import Util.Datei

bank :: P.Type -> Maybe Int -> IO String
bank p res = do
    let it = case res of Just s -> Ok s ; Nothing -> No
    bepunkteStudentDB (P.ident p) (P.anr p) it (P.highscore p)

    pid <- Inter.Store.store p res

    time <- zeit
    let msg = unwords [ time
		      , "(",  pid, ")"
		      , "cgi-" ++ P.matrikel p
		     , "(", P.matrikel p, ")"
		      , P.subject p , ":"
		     , Reporter.Result.result_string res
		     , "\n"
		     ]

    let logcgi = Datei	{ pfad = [ "autotool", "log" ], name = "CGI"
			, relativzahl = error "Inter.Bank.relativzahl"
			}
    anhaengen logcgi msg

    return msg


-- wer braucht das? soll Util.Datei benutzen!

-- home :: FilePath -> IO FilePath
-- absolut machen ( $HOME davor )
-- home fp = do
--    user <- Posix.getEffectiveUserName
--    return $ "/home/" ++ user ++ "/" ++ fp

zeit :: IO String
zeit = do
    clock <- getClockTime
    cal <- toCalendarTime clock    
    return $ calendarTimeToString cal