{-# language PatternSignatures #-}

module Inter.Common where

import Inter.Types
import Inter.Bank
import Gateway.CGI
import qualified Inter.Param as P
import qualified Control.Aufgabe as A
import qualified Control.Student as S
import qualified Autolib.Output as O
import Control.Types (toString)
import Challenger.Partial
import Autolib.Reporter
import Autolib.ToDoc ( text )

-- import qualified Text.XHtml
import qualified Autolib.Multilingual.Html as Html

import System.Directory
import qualified Control.Exception

mkpar stud auf = P.empty 
            { P.mmatrikel = Just $ S.mnr stud
	    , P.aufgabe = A.name auf
	    , P.typ = A.typ auf
	    , P.anr = A.anr auf
	    , P.vnr = A.vnr auf
	    , P.highscore = A.highscore auf
	    , P.ident = S.snr stud
            }

make_instant_common vnr manr stud var = 
    make_instant_common_with vnr manr stud var $ toString $ S.mnr stud 

make_instant_common_with vnr manr stud var seed = do
    let p = problem var
    let mat = S.mnr stud
    k <- key var seed
    g <- gen var vnr manr k 
    let ( Just i  , _ :: Html.Html ) = export g
    ( _, icom :: Html.Html) <- run $ report p i
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

-----------------------------------------------------------------------------

footer scores = do
    hr 
    -- h3 "Informationen zum autotool"
    let entry name url = O.Named_Link name url
    embed $ output
          $ foldr1 O.Beside
	      [ O.Text "autotool", O.Text ":"
	      , entry "home" 
		      "http://dfa.imn.htwk-leipzig.de/auto/"
              , O.Text  " / "
	      , entry " bugs " 
		      "http://dfa.imn.htwk-leipzig.de/bugzilla/buglist.cgi?component=autotool&bug_status=NEW&bug_status=ASSIGNED&bug_status=REOPENED"
              , O.Text " / "
	      , entry " highscores" scores
	      ]
    hr

slink = do
    e <- doesFileExist "link.scores"
    scores <- 
        if e
        then readFile "link.scores" >>= return . head . lines
        else return "https://autotool.imn.htwk-leipzig.de/high/score.text"
    return scores


scores_link = io $ slink `Control.Exception.catch` \ e -> return ( show e )