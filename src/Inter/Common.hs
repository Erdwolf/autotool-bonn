module Inter.Common where

import Inter.Types
import Inter.Bank
import Gateway.CGI
import qualified Inter.Param as P
import qualified Control.Aufgabe as A
import qualified Control.Student as S
import Control.Types (toString)
import Challenger.Partial

import qualified Text.Html
import Autolib.Reporter

mkpar stud auf = P.empty 
            { P.mmatrikel = Just $ S.mnr stud
	    , P.aufgabe = A.name auf
	    , P.typ = A.typ auf
	    , P.anr = A.anr auf
	    , P.vnr = A.vnr auf
	    , P.highscore = A.highscore auf
	    , P.ident = S.snr stud
            }

make_instant_common vnr manr stud var = do
    let p = problem var
    let mat = S.mnr stud
    k <- key var $ toString mat 
    g <- gen var vnr manr k 
    let ( Just i  , _ :: Text.Html.Html ) = export g
    ( _, icom :: Text.Html.Html) <- run $ report p i
    return ( p, i, icom )

-- | erreichte punkte in datenbank schreiben 
-- und lösung abspeichern
punkte tutor stud auf ( minst, mcs, mres, com ) = 
     if tutor || A.current auf
	then do
             hr ; h3 "Eintrag ins Logfile"
	     let p = ( mkpar stud auf )  
		     { P.minstant = minst
		     , P.input = mcs
		     , P.report = com
		     , P.mresult = mres 
		     }
	     msg <- io $ bank p
	     pre msg
	     return ()
        else vorbei


vorbei = do
    h3 "Einsendeschluß dieser Aufgabe ist überschritten"
    plain "Einsendung wird nicht gespeichert, Bewertung wird ignoriert."

pure_punkte tutor stud auf ( minst, mcs, mres, com ) = 
     when ( tutor || A.current auf ) $ do
	     let p = ( mkpar stud auf )  
		     { P.minstant = minst
		     , P.input = mcs
		     , P.report = com
		     , P.mresult = mres 
		     }
	     bank p
             return ()
