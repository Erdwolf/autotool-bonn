module Inter.Bank where

-- $Id$

import SQLqueries
import qualified Posix

import Time
import qualified Inter.Param as P
import qualified Reporter.Result

bank :: P.Type -> Maybe Int -> IO String
bank p res = do
    let it = case res of Just s -> Ok s ; Nothing -> No
    bepunkteStudentDB (P.ident p) (P.anr p) it (P.highscore p)

    time <- zeit
    let msg = unwords [ time
		      , "(",  "cgi", ")"
		      , "cgi-" ++ P.matrikel p
		     , "(", P.matrikel p, ")"
		      , P.subject p , ":"
		     , Reporter.Result.result_string res
		     , "\n"
		     ]
    rf <- home  "autotool/log/CGI" --  TODO: konfigurierbar machen!
    appendFile rf  msg	
    return msg


home :: FilePath -> IO FilePath
-- absolut machen ( $HOME davor )
home fp = do
    user <- Posix.getEffectiveUserName
    return $ "/home/" ++ user ++ "/" ++ fp

zeit :: IO String
zeit = do
    clock <- getClockTime
    cal <- toCalendarTime clock    
    return $ calendarTimeToString cal