-- | edit the problem configuration
-- and check a sample solution

--  $Id$

module Main where

import Inter.CGI
import Inter.Evaluate
import Inter.Make 
import Inter.Bank
import Inter.Store 
import Inter.Login
import qualified Inter.Param as P

import Control.Types 
    ( toString, fromCGI, Name, Remark, HiLo (..), Status (..)
    , Oks (..), Nos (..), Time , Wert (..), MNr, SNr
    )


import qualified Inter.Collector


import Challenger.Partial
import Inter.Types

import Control.Student.CGI
import Control.Vorlesung.DB
import qualified Control.Punkt
import qualified Control.Stud_Aufg.DB

import qualified Control.Aufgabe as A
import qualified Control.Stud_Aufg as SA
import qualified Control.Student.Type as S
import qualified Control.Vorlesung as V

import Autolib.Reporter.Type
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Util.Sort
import Autolib.FiniteMap

import Random
import Data.Typeable
import Data.Maybe
import Data.List ( partition )
import Control.Monad
import qualified Control.Exception

import Text.Html ( Html )

main :: IO ()
main = Inter.CGI.execute "Super.cgi" 
     $ iface Inter.Collector.makers

iface :: [ Make ] -> Form IO ()
iface mks = do

    h3 "Login und Auswahl der Vorlesung"
    open btable
    ( stud, vnr, tutor ) <- Inter.Login.form
    let snr = S.snr stud
    when ( not tutor ) $ close -- table

    -- das sind alle aufgaben 
    aufs <- io $ A.get $ Just vnr
    let opts = do
             auf <- aufs
             return ( toString $ A.name auf , Just $ auf )

    ( mauf, del ) <- 
        if tutor
	   then do 
	        selector_edit_delete "anr" "Aufgabe" 0 
                   $ ( "(neue Aufgabe)", Nothing ) : opts
           else do 
		auf <- statistik stud aufs
                return ( Just auf, False )
    when tutor $ close -- btable ??

    case mauf of
         Just auf | tutor -> tutor_statistik vnr auf
         _ -> return ()

    let manr = fmap A.anr mauf
    
    when del $ do
        Just anr <- return manr
        io $ A.delete anr
        plain $ unwords [ "Aufgabe", show anr, "gelöscht." ]
	mzero

    ( mk, type_click ) <- find_mk mks tutor mauf

    auf' <- if tutor 
            then do
		 edit_aufgabe mk mauf vnr manr type_click
	    else -- kein tutor 
                case mauf of
		  Nothing -> do 
                      -- kommt eigentlich nicht vor?
		      plain "keine Aufgabe gewählt"
		      mzero
		  Just auf -> do
                      return auf
    stud' <- get_stud tutor stud
    hr
    ( cs, res ) <- solution vnr manr stud' mk auf' 
    -- bewertung in DB (für Stud-Variante)
    when ( not tutor ) $ punkte stud' auf' ( cs, res )
    -- when ( not tutor ) $ statistik stud' aufs
    return ()

-------------------------------------------------------------------------

-- | bestimme aufgaben-typ (maker)
-- für tutor: wählbar
-- für student: fixiert (ohne dialog)
find_mk mks tutor mauf = do
    let pre_mk = fmap (toString . A.typ) mauf
    let handler pre_mk opts = 
            if tutor 
            then do
		 hr
		 h3 "Parameter dieser Aufgabe:"
		 open btable -- will be closed in edit_aufgabe (tutor branch)
		 selector_submit_click "typ" "Typ" pre_mk opts
            else return 
		    ( fromMaybe (error "oof") $ do
                          pre <- pre_mk ; lookup pre opts
                    , False
		    ) 
    handler pre_mk $ do
        mk <- mks
	return ( show mk, mk )

-- | ändere aufgaben-konfiguration (nur für tutor)
edit_aufgabe mk mauf vnr manr type_click = do
    case mk of 
        Make doc ( fun :: conf -> Var p i b ) ex -> do
            ( name :: Name ) <- fmap fromCGI 
		     $ defaulted_textfield "Name" 
		     $ case mauf of Nothing -> "noch kein Name"
                                    Just auf -> toString $ A.name auf
	    ( remark :: Remark ) <- fmap fromCGI 
		    $ defaulted_textarea "Remark" 
		    $ case mauf of Nothing -> "noch keine Hinweise"
	                           Just auf -> toString $ A.remark auf
            open row
            plain "Highscore"
	    ( mhilo :: Maybe HiLo ) <- selector' "Highscore"  
                ( case mauf of Nothing -> "XX"
		               Just auf -> show $ A.highscore auf )
                   $ do
		     ( x :: HiLo ) <- [ minBound .. maxBound ]
                     return ( show x, x )
            close -- row
            open row
            plain "Status"
	    ( mstatus :: Maybe Status ) <- selector' "Status"  
                ( case mauf of Nothing -> "XX"
		               Just auf -> show $ A.status auf )
                   $ do
		     ( x :: Status ) <- [ minBound .. maxBound ]
                     return ( show x, x )
            close -- row
            -- FIXME: get now() from DB
            ( von :: Time ) <- fmap fromCGI 
		    $ defaulted_textfield "von" 
		    $ case mauf of Nothing -> "2004-10-15 11:16:08"
				   Just auf -> toString $ A.von auf
            ( bis ::Time ) <- fmap fromCGI
		    $ defaulted_textfield "bis" 
		    $ case mauf of Nothing -> "2004-10-15 11:16:08"
				   Just auf -> toString $ A.bis auf

            -- nimm default-config, falls type change
            conf <- editor_submit "conf" "Konfiguration" 
		    $ case mauf of 
			  Just auf | not type_click  -> 
				 read $ toString $ A.config auf
			  _ -> ex :: conf
	    close -- table
				   
            br
	    up <- submit "update" "update data base: aufgabe"
            let auf' = A.Aufgabe 
		               { A.anr = error "Super.anr" -- intentionally
			       , A.vnr = vnr
			       , A.name = name
			       , A.typ = fromCGI $ show mk
			       , A.config = fromCGI $ show conf
			       , A.remark = remark
			       , A.highscore = fromMaybe Keine mhilo
			       , A.status = fromMaybe Demo mstatus
			       , A.von = von
			       , A.bis = bis
			       , A.current = False -- ist egal
			       }
            when up $ io $ A.put manr auf'
            return auf'

-- | matrikelnummer zum aufgabenlösen:
-- tutor bekommt eine gewürfelt (und kann neu würfeln)
-- student bekommt genau seine eigene
get_stud tutor stud = 
    if tutor 
       then do
         hr
	 m0 <- io $ randomRIO (0, 999999 :: Int) 
	 -- neu würfeln nur bei änderungen oberhalb von hier
	 plain "eine gewürfelte Matrikelnummer:"
	 mat <- with ( show m0 ) $ textfield "mat" ( show m0 )
         -- falls tutor, dann geht es hier nur um die matrikelnr
	 return $ stud { S.mnr = fromCGI mat
		       , S.snr = error "gibt es nicht"
		       }
       else do
	 return stud

-- | eingabe und bewertung der lösung
-- für tutor zum ausprobieren
-- für student echt
solution vnr manr stud ( Make doc ( fun :: conf -> Var p i b ) ex ) auf = do

    let conf = read $ toString $ A.config auf
        var = fun  conf
        p = problem var
    let mat = S.mnr stud
    k <- io $ key var $ toString mat 
    g <- io $ gen var vnr manr k
    let ( Just i  , com :: Doc ) = export g
        ini  = initial  (problem var) i
        -- desc = describe (problem var) i
    ( _ , desc :: Html ) <- io $ run $ report (problem var) i
    br
    parameter_table auf
    h3 "Aufgabenstellung"
    html desc
    hr ---------------------------------------------------------
    h3 "Lösung"

    plain "Eingabefeld:"
    ex   <- submit "subex" "example"
    prev <- submit "subprev" "previous"
    esub  <- submit "subsol" "submit"
    br
    when ( ex || prev ) blank

    let b0 = render $ toDoc ini 
    def <- io $ if prev 
	   then Inter.Store.latest (mkpar stud auf)
                      `Control.Exception.catch` \ _ -> return b0
	   else return b0
    sol <- textarea "sol" def
    br
    plain "oder Datei-Upload (note: currently broken):"
    up <- file "up" undefined
    fsub  <- submit "fsub" "submit"
    br

    cs <- if esub
          then do Just cs <- return sol ; return cs
	  else if fsub
	       then do Just cs <- return up ; return cs
	       else mzero

    hr
    h3 "Bewertung"
    -- let (res, com :: Html) = export $ evaluate p i cs
    (res, com :: Html) <- io $ run $ evaluate p i cs
    html com
    return ( cs, res )

parameter_table auf = do
    h3 $ unwords [ "Aufgabe", toString $ A.name auf ]
    let tab = do
            open btable
            sequence_ $ do 
              ( name, thing ) <- 
            	    [ ( "Status", toString $ A.status auf )
            	    , ( "von", toString $ A.von auf )
            	    , ( "bis", toString $ A.bis auf )
            	    , ( "Highscore", toString $ A.highscore auf )
            	    ]
              return $ do
            	    open row 
            	    plain name
            	    plain thing
            	    close 
            close -- table
    let hin = above ( plain "Hinweise" )
	            ( pre $ toString $ A.remark auf )
    beside tab hin

-- | erreichte punkte in datenbank schreiben 
-- und lösung abspeichern
punkte stud auf ( cs, res ) = do
     hr
     let p = ( mkpar stud auf )  { P.input = cs }
     msg <- io $ bank p res
     h3 "Eintrag ins Logfile:"
     pre msg
     return ()

mkpar stud auf = P.empty 
            { P.mmatrikel = Just $ S.mnr stud
	    , P.aufgabe = A.name auf
	    , P.typ = A.typ auf
	    , P.anr = A.anr auf
	    , P.vnr = A.vnr auf
	    , P.highscore = A.highscore auf
	    , P.ident = S.snr stud
            }

------------------------------------------------------------------------

-- | für Student: statistik aller seiner Aufgaben anzeigen, 
-- mit aufgabenauswahl
statistik stud aufs = do
    hr 
    h3 "Punktestand und Aufgaben-Auswahl"
    -- daten holen
    score <- io $ sequence $ do
        auf <- aufs
	return $ do
            sas <- SA.get_snr_anr (S.snr stud) (A.anr auf) 
            let okno = case sas of
		     [    ] ->  ( Oks 0, Nos 0 )
		     [ sa ] ->  ( SA.ok sa, SA.no sa )
	    return ( auf, okno )
    -- vorige aufgabe holen
    mvor <- look "vor"
    -- daten anzeigen
    let dat = do
            open btable
            clicks <- sequence $ do 
                ( auf, (ok, no) ) <- score
                let name = toString $ A.name  auf
                return $ do
            	    open row
                    let col = if A.current auf
			         && Mandatory == A.status auf
			      then if ok > Oks 0
				   then "green"
				   else "red"
			      else "black"
                    plain name
                    farbe col $ toString $ A.status auf
            	    farbe col $ show ok 
            	    farbe col $ show no
                    click <- if A.current auf
                       then do submit name "go"
            	       else do plain name ; return False
                    farbe col $ toString $ A.von auf
                    farbe col $ toString $ A.bis auf
            	    close
                    return [ ( click, auf ) | click || mvor == Just name ]
            close
            return clicks
    -- auswerten
    let goal = sum $ do 
            ( auf, okno ) <- score
	    guard $ A.status auf == Mandatory
	    return ( 1 :: Int )
	done = sum $ do 
            ( auf, (ok, no) ) <- score
	    guard $ A.status auf == Mandatory
	    guard $ ok > Oks 0
	    return ( 1 :: Int )
        percent = ( 100 * done ) `div` goal
        -- anzeigen
        aus = when ( goal > 0 ) $ do
	    plain $ unwords 
                  [ "Von", show goal, "Pflicht-Aufgaben" 
		  , "haben Sie bis jetzt", show done, "erledigt."
		  , "Das sind", show percent, "Prozent." 
		  ]

    clicks <- dat ; br ; aus

    let ( cli, uncli ) = partition fst $ concat clicks
        clicked = map snd cli
        unclicked = map snd uncli
    auf <- case clicked of
        [ auf ] -> do
            -- plain "neue aufgabe, gedächtnis löschen"
	    blank 
            return auf
        _ -> case unclicked of
	      [ auf ] -> do
		   -- plain "alte aufgabe, weiter"
		   return auf 
              _ -> do
		   -- plain "gar keine aufgabe, stop"
                   mzero 
    hidden "vor" $ toString $ A.name auf
    return auf

--------------------------------------------------------------------------

data Entry = Entry
	   { snr :: SNr
	   , mnr :: MNr
	   , vorname :: Name
	   , name :: Name
	   , oks :: Oks
	   , nos :: Nos
	   }

-- | für Tutor: statistik aller einsendungen zu dieser Aufgabe
-- mit Möglichkeit, Bewertungen zu ändern

tutor_statistik vnr auf = do
    hr
    t <- io $ Control.Vorlesung.DB.teilnehmer vnr
    saufs <- io $ Control.Stud_Aufg.DB.get_anr $ A.anr auf
    let fm = listToFM $ do
            sauf <- saufs
	    return ( SA.snr sauf , ( SA.ok sauf, SA.no sauf ) )
    let entries = do
	    ( s, ( m, v, n ) ) <- t
            let ( ok, no ) = lookupWithDefaultFM fm ( Oks 0, Nos 0 ) s
	    return $ Entry 
		   { snr = s, mnr = m 
		   , vorname = v, name = n
		   , oks = ok, nos = no
		   }
    h3 "Statistik für diese Aufgabe"
    open btable
    actions <- sequence $ do
        e <- sortBy name entries
        return $ do
	    open row
	    plain $ toString $ name e 
	    plain $ toString $ vorname e 
	    plain $ toString $ mnr e
            let status = oks e > Oks 0
            let checkname = "st" ++ ( toString $ mnr e ) 
            check <- checkbox status checkname ""
            let actions = do
		guard $ check /= status 
                return $ case check of
		    True -> Control.Punkt.set 
			      ( snr e ) ( A.anr auf ) ( Ok 0 )
		    False -> Control.Punkt.set 
			      ( snr e ) ( A.anr auf ) Reset

            close -- row
            return $ sequence actions
    close -- btable
    sub <- submit "update" "update data base: stud_aufg ..."
    when sub $ do
        io $ sequence_ actions
	plain "... done (refresh picture)"
    return ()


