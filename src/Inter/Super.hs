-- | edit the problem configuration
-- and check a sample solution

-- TODO: hack this module into pieces

--  $Id$

module Main where

import Inter.CGI
import Inter.Evaluate
import Inter.Make 
import Inter.Bank
import Inter.Store 
import Inter.Login
import Inter.Logged
import qualified Inter.Param as P

import Control.Types 
    ( toString, fromCGI, Name, Remark, HiLo (..), Status (..)
    , Oks (..), Nos (..), Time , Wert (..), MNr, SNr, VNr, ANr
    )


import qualified Inter.Collector


import Challenger.Partial
import Inter.Types

import Control.Student.CGI
import Control.Vorlesung.DB
import qualified Control.Student.DB
import qualified Control.Punkt
import qualified Control.Stud_Aufg.DB

import qualified Control.Aufgabe as A
import qualified Control.Stud_Aufg as SA
import qualified Control.Student as S
import qualified Control.Vorlesung as V

import Autolib.Reporter.Type hiding ( wrap )
import Autolib.ToDoc
import qualified Autolib.Output as O
import Autolib.Reader
import Autolib.Util.Sort
import Autolib.FiniteMap

import Debug

import System.Random
import Data.Typeable
import Data.Maybe
import Data.List ( partition )
import Control.Monad
import qualified Control.Exception

import Text.Html ( Html, primHtml )

main :: IO ()
main = Inter.CGI.execute "Super.cgi" $ do
    wrap $ iface Inter.Collector.makers
    footer

iface :: [ Make ] -> Form IO ()
iface mks = do

    h3 "Login und Auswahl der Vorlesung"

    ( stud, vnr, tutor ) <- Inter.Login.form

    let snr = S.snr stud

    -- das sind alle aufgaben 
    aufs <- io $ A.get $ Just vnr
    let opts = do
             auf <- aufs
             return ( toString $ A.name auf , Just $ auf )

    ( mauf , action ) <- 
        if tutor
	   then do 
                open btable
	        mauf <- click_choice "Aufgabe"
                         $ ( "(neue Aufgabe)", Nothing ) : opts
                action <- click_choice "Action"
			  $ do act <- [ Config, Statistics, Delete ] 
			       return ( show act, act )
                return ( mauf, action )
           else do 
		( action, auf ) <- statistik False stud aufs
                return ( Just auf, action )

    when ( tutor && Statistics == action ) $ do
         Just auf <- return mauf 
	 ( act, sauf ) <- tutor_statistik vnr auf
         mtriple <- show_previous ( Edit == act ) sauf
         case mtriple of
	     Nothing -> return ()
	     Just ( cs, res, com ) -> do
                 [ stud ] <- io $ S.get_snr $ SA.snr sauf
		 punkte stud auf ( cs, res, com )
	 mzero

    let manr = fmap A.anr mauf
    
    when ( Delete == action ) $ do
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

    case action of
        Config -> do
	    solution vnr manr stud' mk auf' 
	    return ()
        Solve -> do
            ( cs, res, com ) <- solution vnr manr stud' mk auf' 
	    punkte stud' auf' ( cs, res, com )
	Edit | tutor -> do
	    find_previous True stud' auf'
            return ()
	View -> do
	    find_previous False stud' auf'
            return ()

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
		 selector_submit_click "Typ" pre_mk opts
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
	    ( mhilo :: Maybe HiLo ) <- selector' 
                ( case mauf of Nothing -> "XX"
		               Just auf -> show $ A.highscore auf )
                   $ do
		     ( x :: HiLo ) <- [ minBound .. maxBound ]
                     return ( show x, x )
            close -- row
            open row
            plain "Status"
	    ( mstatus :: Maybe Status ) <- selector' 
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
            conf <- editor_submit "Konfiguration" 
		    $ case mauf of 
			  Just auf | not type_click  -> 
				 read $ toString $ A.config auf
			  _ -> ex :: conf
	    close -- table
				   
            br
	    up <- submit "update data base: aufgabe"
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
	 mat <- with ( show m0 ) $ textfield ( show m0 )
         -- falls tutor, dann geht es hier nur um die matrikelnr
	 return $ stud { S.mnr = fromCGI mat
		       , S.snr = error "gibt es nicht"
		       }
       else do
	 return stud


find_previous edit stud auf = do

    hr ---------------------------------------------------------

    -- kann sein, daß S.anr  error  ergibt (für tutor)
    sas <- io $ SA.get_snr_anr (S.snr stud) (A.anr auf) 
                   `Control.Exception.catch` \ any -> return []
    case sas of
        [ sa ] -> show_previous edit sa
        _ -> return Nothing

-- | TODO: possibly with edit (for tutor)
show_previous edit sa = do

    hr ;  h3 "Vorige Einsendung und Bewertung zu dieser Aufgabe"
    -- pre $ show sa
    br ; plain "Einsendung:"
    case SA.input sa of
        Just file -> do
            cs <- io $ logged "Super.view" 
    	         $ readFile $ toString file
    	    pre cs
        Nothing -> plain "(keine)"
    br
    plain "Bewertung:"
    h <- case SA.report sa of
        Just file -> do
            -- alte bewertung ist schon da
	    io $ readFile $ toString file
        Nothing -> case edit of
            -- alte bewertung nicht da
	    False -> return "(keine Bewertung)"
            True -> case SA.input sa of
                 -- stattdessen alte eingabe lesen
                 Just file -> io $ readFile $ toString file
		 Nothing -> return "(keine Eingabe)"
    case edit of
	 False -> do
             html $ primHtml h
             blank -- ??
	     return Nothing
         True  -> do
               mcom <- textarea h
               let com = fromMaybe h mcom   
               open table
               grade <- click_choice0  "Grade" 
			[ ("Pending", Pending), ("Ok", Ok 1), ("No", No) ]
               close -- table
	       return $ Just ( Nothing
			     , grade
			     , case grade of 
			           Pending -> Nothing 
			           _       -> Just $ primHtml com 
			     )




-- | eingabe und bewertung der lösung
-- für tutor zum ausprobieren
-- für student echt
solution vnr manr stud 
         ( Make doc ( fun :: conf -> Var p i b ) ex ) auf = do

    let conf = read $ toString $ A.config auf
        var = fun  conf
        p = problem var
        past = mkpar stud auf

    let mat = S.mnr stud
    k <- io $ key var $ toString mat 
    g <- io $ gen var vnr manr k
    let ( Just i  , com :: Doc ) = export g
        ini  = initial  (problem var) i
    br
    parameter_table auf

    h3 "Aufgabenstellung"
    embed $ report (problem var) i

    ---------------------------------------------------------
    h3 "Neue Einsendung"

    -- das vorige mal bei eingabefeld oder upload?
    epeek <- look "subsol"
    fpeek <- look "fsub"

    esol <- if isJust fpeek
       then do 
          -- voriges mal file gewählt, also textarea nicht anzeigen
          wef <- submit "Text-Eingabefeld" 
          when wef $ blank
          return Nothing
       else do
          plain "Text-Eingabefeld:"
	  ex    <- submit "Beispiel laden"
	  prev  <- submit "vorige Einsendung laden"
	  esub  <- submit "Textfeld absenden"
	  br
	  when ( ex || prev ) blank

          let b0 = render $ toDoc ini 
	  def <- io $ if prev 
	      then Inter.Store.latest Inter.Store.Input past
                      `Control.Exception.catch` \ _ -> return b0
	      else return b0
          sol <- textarea def
          return sol

    br ; plain "oder " 

    fsol <- if isJust epeek
        then do
	    -- voriges mal textfeld gewählt, also file-dialog nicht anzeigen
            wup <- submit "Datei hochladen"
            when wup $ blank
            return Nothing
        else do
            plain "Datei auswählen:"
            up <- file undefined
	    fsub  <- submit "Datei absenden"
	    return $ if fsub then up else Nothing

    cs <- case esol of
             Just cs -> return cs
	     Nothing -> case fsol of
		  Just cs -> return cs
		  Nothing -> mzero

    hr ; h3 "Neue Bewertung"
    (res, com :: Html) <- io $ run $ evaluate p i cs
    html com
    return ( Just cs, fromMaybe No res, Just com )

parameter_table auf = do
    h3 $ unwords [ "Aufgabe", toString $ A.name auf ]
    above ( plain "Hinweise" )
	            ( pre $ toString $ A.remark auf )

-- | erreichte punkte in datenbank schreiben 
-- und lösung abspeichern
punkte stud auf ( mcs, res, com ) = do
     hr ; h3 "Eintrag ins Logfile"
     let p = ( mkpar stud auf )  
	   { P.input = mcs, P.report = com, P.result = res }
     msg <- io $ bank p
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

data Action = Solve  -- ^ neue Lösung bearbeiten
	    | View -- ^ alte Lösung + Bewertung ansehen
	    | Edit -- ^ alte Lösung + Bewertung ändern
	    | Statistics 
	    | Config
            | Delete 
     deriving ( Show, Eq, Typeable )

-- | für Student: statistik aller seiner Aufgaben anzeigen, 
-- für Tutor: kann Aufgabenlösung sehen und (nach-)korrigieren
-- mit aufgabenauswahl
statistik tutor stud aufs = do
    hr 
    h3 "Punktestand und Aufgaben-Auswahl"
    -- daten holen
    score <- io $ sequence $ do
        auf <- aufs
	return $ do
            sas <- SA.get_snr_anr (S.snr stud) (A.anr auf) 
            let okno = case sas of
		     [    ] ->  ( Oks 0, Nos 0 , Nothing )
		     [ sa ] ->  ( SA.ok sa, SA.no sa, SA.result sa )
	    return ( auf, okno )

    -- daten anzeigen
    let dat = do
	    begin -- mutex
            open btable
            open row
            plain "Aufgabe" ; plain "Status" ; plain "Highscore"
            plain "von" ; plain "bis"
            plain "vorige Bewertung" ; plain "Gesamt-Wertungen"
            close -- row
            sequence_ $ do 
                ( auf, ( ok, no, mres ) ) <- score
                let name = toString $ A.name  auf
                return $ do
            	    open row
                    let col = if A.current auf
			         && Mandatory == A.status auf
			      then if ok > Oks 0
				   then "green"
				   else "red"
			      else "black"
                    farbe col $ toString $ A.name auf
                    farbe col $ toString $ A.status auf
                    farbe col $ toString $ A.highscore auf
                    farbe col $ toString $ A.von auf
                    farbe col $ toString $ A.bis auf
                    farbe col $ show mres
            	    farbe col $ show ( ok , no )
                    sequence_ $ do
		        ch <- if tutor then        [ View, Edit ]
			               else [ Solve, View ]
                        return $ click ( show ch , ( ch, auf ))
            	    close -- row
            close -- table
            end -- mutex
    -- auswerten
    let goal = sum $ do 
            ( auf, okno ) <- score
	    guard $ A.status auf == Mandatory
	    return ( 1 :: Int )
	done = sum $ do 
            ( auf, (ok, no, mres) ) <- score
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

    ( ch, auf ) <- dat ; br ; aus
    return ( ch, auf )

--------------------------------------------------------------------------

data Entry = Entry
	   { snr :: SNr
	   , mnr :: MNr
	   , vorname :: Name
	   , name :: Name
	   , oks :: Oks
	   , nos :: Nos
	   }


---------------------------------------------------------------------------

-- | will return Maybe Stud_Aufg for re-grading
tutor_statistik :: VNr -> A.Aufgabe 
		-> Form IO ( Action, SA.Stud_Aufg )
tutor_statistik vnr auf = do
    hr
    saufs <- io $ Control.Stud_Aufg.DB.get_anr $ A.anr auf

    h3 "Statistik für diese Aufgabe"
    open btable
    begin -- mutex

    open row
    plain "Matrikel" ; plain "Vorname" ; plain "Name" 
    plain "Oks" ; plain "Nos" ; plain "Result"
    plain "Action"
    close -- row

    sequence_ $ do
        sauf <- saufs
        return $ do
            [ stud ] <- io $ Control.Student.DB.get_snr $ SA.snr sauf
	    open row
	    plain $ toString $ S.mnr stud
	    plain $ toString $ S.vorname stud
	    plain $ toString $ S.name stud
            plain $ toString $ SA.ok sauf
	    plain $ toString $ SA.no sauf
	    plain $ show $ SA.result sauf
	    click <- case SA.result sauf of
	        Nothing -> do plain "" ; plain ""
		Just w  -> do
		    click ( "View", ( View, sauf ))
		    click ( "Edit",  ( Edit, sauf ))
            close -- row
    close -- btable
    end -- mutex

-----------------------------------------------------------------------------

footer = do
    hr ; h3 "Informationen zum autotool"
    let entry name url =
            O.Beside ( O.Text name ) ( O.Link url )
    embed $ output
          $ O.Itemize
	      [ entry "home: " 
		      "http://141.57.11.163/auto/"
	      , entry "bugs (bekannte ansehen und neue melden): " 
		      "http://141.57.11.163/cgi-bin/bugzilla/buglist.cgi?value-0-0-0=autotool"
	      , entry "scores: " 
		      "http://www.imn.htwk-leipzig.de/~autotool/scores"
	      ]
    hr



