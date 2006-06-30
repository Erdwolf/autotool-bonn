-- | edit the problem configuration
-- and check a sample solution

-- TODO: hack this module into pieces

--  $Id$

module Main where

import Gateway.CGI
import Inter.Evaluate
import Inter.Make 
import Inter.Motd
import Inter.Bank
import Inter.Store 
import Inter.Login
import Inter.Logged
import qualified Inter.Param as P
import qualified Inter.Statistik

import Gateway.Help

import Autolib.Set
import qualified Autolib.Output

import Control.Types 
    ( toString, fromCGI, Name, Typ , Remark, HiLo (..), Status (..)
    , Oks (..), Nos (..), Time , Wert (..), MNr, SNr, VNr, ANr, UNr
    , TimeStatus (..)
    )

import qualified Control.Types   


import qualified Inter.Collector


import Challenger.Partial
import Inter.Types
import Inter.Common

import Control.Student.CGI
import Control.Vorlesung.DB
import qualified Control.Student.DB
import qualified Control.Punkt
import qualified Control.Stud_Aufg.DB

import qualified Control.Aufgabe as A
import qualified Control.Stud_Aufg as SA
import qualified Control.Student as S
import qualified Control.Vorlesung as V
import qualified Control.Gruppe as G
import qualified Control.Stud_Grp as SG
import qualified Control.Schule as U

import Autolib.Reporter.Type hiding ( wrap, initial )
import Autolib.ToDoc
import qualified Autolib.Output as O
import Autolib.Reader
import Autolib.Util.Sort
import Autolib.FiniteMap

import qualified Util.Datei as D
import Debug
import qualified Local

import System.Random
import System.Directory
import Data.Typeable
import Data.Maybe
import Data.Tree
import Data.List ( partition )
import Control.Monad
import qualified Control.Exception

import qualified Text.Html

import Inter.DateTime ( defaults )

main :: IO ()
main = Gateway.CGI.execute ( Local.cgi_name ++ "#hotspot" ) $ do
   wrap $ iface $ Inter.Collector.tmakers
   scores <- io $ slink `Control.Exception.catch` \ e -> return ( show e )
   footer scores

slink = do
    e <- doesFileExist "link.scores"
    scores <- 
        if e
        then readFile "link.scores" >>= return . head . lines
        else return "https://autotool.imn.htwk-leipzig.de/high/score.text"
    return scores



iface :: Tree ( Either String Make ) -> Form IO ()
iface tmk = do

    new <- click_choice_with_default 0 "Aktion" 
        [ ( "Account benutzen", False ) 
	, ( "Account anlegen", True )
	]

    if new 
       then edit_create Nothing
       else use_account tmk

data Code = Stat | Auf | Einsch
   deriving ( Show, Eq, Typeable )

use_account tmk = do

    h3 "Login"
    -- f�r Student und Tutor gleicher Start

    svt @ ( stud, vor, tutor0, attends0 ) <- Inter.Login.form

    ( tutor, attends ) <- 
        if tutor0
        then do
            h3 "Sie sind Tutor f�r diese Vorlesung."
            open btable
            result <- click_choice_with_default 0 "Arbeiten als..."
                  [ ("Tutor", ( tutor0, attends0 ) ) 
                  , ("Student", ( False, True ) )
                  ]
            close
            return result
        else return ( False, attends0 )

    open btable
    aktion <- click_choice_with_default 0 "Aktion" $
           [ ("Aufgaben",  aufgaben tmk ( stud, V.vnr vor, tutor )  ) 
                 | attends || tutor ]
	++ [ ("Einschreibung", veranstaltungen ( stud, vor, tutor )  ) ]
        ++ [ ("Statistiken",   Inter.Statistik.main svt  ) | tutor ]
        ++ [ ("Waisenkinder", waisenkinder $ S.unr stud ) | tutor ]
    close -- btable 
    aktion

-- | Studenten behandeln, die in keiner �bungsgruppe sind
waisenkinder :: UNr -> Form IO ()
waisenkinder u = do
    h3 $ "Waisenkinder"
    plain $  "Studenten Ihrer Schule, die keine �bungsgruppe gew�hlt haben"
    studs <- io $ S.orphans $ u
    open btable
    Inter.Statistik.edit_studenten studs

-- | alle �bungen,
-- markiere besuchte �bungen
-- one-click f�r verlassen\/besuchen 
veranstaltungen :: ( S.Student , V.Vorlesung , Bool ) -> Form IO ()
veranstaltungen ( stud , vor, False ) = do
    h3 "Einschreibung"

    -- dieser student f�r diese Vorlesung
    ags <- io $ G.get_attended ( V.vnr vor ) ( S.snr stud )

    case ags of
        [] -> plain "Sie sind in keine �bungsgruppe eingeschrieben."
        _  -> show_gruppen "Sie sind eingeschrieben in �bungsgruppe:" ags

    -- alle Gruppen f�r diese Vorlesung
    gs <- io $ G.get_this $ V.vnr vor
    show_gruppen "alle �bungsgruppen zu dieser Vorlesung:" gs

    when ( Control.Types.Current /= V.einschreib vor ) $ do
         br
	 plain $ unlines [ "Das Ein/Ausschreiben ist"
			 , "nur von " ++ show ( V.einschreibVon vor )
			 , "bis " ++ show ( V.einschreibBis vor )
			 , "m�glich."
			 ]
	 mzero
    opts <- sequence $ do
        g <- gs
	return $ do
	    att <- io $ SG.attendance ( G.gnr g )
	    let here = G.gnr g `elem` map G.gnr ags
		msg  = toString ( G.name g )
                     ++ " "
		     ++ if here then "verlassen" else "besuchen"
	    return ( msg , ( G.gnr g, here, att >= G.maxStudents g ) )
    open btable 
    ( g, here, full ) <- click_choice "Gruppe" $ opts
    close -- btable
    if here 
       then do 
           par
	   plain $ "click auf besuchte Gruppe: abmelden"
           io $ SG.delete ( S.snr stud ) ( g )
       else do
           par
           plain $ "click auf nicht besuchte Gruppe: anmelden"
	   
	   when full $ do
	        plain $ "Diese Gruppe ist voll."
		mzero
           io $ SG.insert ( S.snr stud ) ( g )
           sequence_ $ do
		a <- ags
		return $ io $ SG.delete ( S.snr stud ) ( G.gnr a )
    return ()

veranstaltungen ( stud , vor, True ) = do
    h3 "Daten der Vorlesung"
    V.edit ( V.unr vor ) ( Just vor )
    
    h3 "�bungsgruppen zu dieser Vorlesung:"
    gs <- io $ G.get_this $ V.vnr vor

    open btable
    act <- click_choice "Aktion:"
        [ ("anzeigen", View )
        , ("erzeugen", Add )
	, ("bearbeiten", Edit )
	, ("l�schen", Delete )
	]
    close -- btable
    case act of
	 View -> do
             show_gruppen "�bungsgruppen zu dieser Vorlesung:" gs
	 Add -> do
	     G.edit ( V.vnr vor ) Nothing
	 Edit -> do
             open btable
             g <- click_choice "Gruppe" $ do
	         g <- gs
		 return ( toString $ G.name g , g )
             close
	     G.edit ( V.vnr vor ) ( Just g )
	 Delete -> do
             open btable
             g <- click_choice "Gruppe" $ do
	         g <- gs
		 return ( toString $ G.name g , g )
             open row
	     click <- submit "wirklich l�schen?"
             close
             close
	     io $ G.delete $ G.gnr g

show_gruppen header gs = do
     plain $ header
     open btable
     open row
     plain "Name" ; plain "Referent"
     plain "Studenten (jetzt)"
     plain "Studenten (maximal)"
     close
     sequence_ $ do
         g <- gs
         return $ do
             open row
             plain $ toString $ G.name g
             plain $ toString $ G.referent g
             c <- io $ SG.attendance $ G.gnr g
             plain $ show c
             plain $ toString $ G.maxStudents g
             close -- row
     close -- btable
    


----------------------------------------------------------------------------

aufgaben tmk ( stud, vnr, tutor ) = do
    h3 "Aufgaben"

    let mks = do Right mk <- flatten tmk ; return mk

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
			  $ do act <- [ Statistics, Config, Delete ] 
			       return ( show act, act )
		close -- btable
                return ( mauf, action )
           else do 
		( action, auf ) <- statistik False stud aufs
                return ( Just auf, action )

    when ( tutor && Statistics == action ) $ do
         Just auf <- return mauf 
	 ( act, sauf, stud ) <- tutor_statistik vnr auf

	 case act of 
	     Rescore rsc -> do
		  sequence_ $ do
		      ( w, sauf, stud ) <- rsc
		      return $ do
		          Inter.Common.punkte tutor stud auf
			      ( Nothing, Nothing, Just w
			      , Just $ Text.Html.primHtml 
					    "Bewertung durch Tutor"  
			      )

             Clear_Cache -> do
                  let d =  D.Datei { D.pfad = [ "autotool", "cache"
				   , toString vnr
				   , toString $ SA.anr sauf
				   ]
			  , D.name = toString $ S.mnr stud 
			  , D.extension = "cache"
			  } 
		  io $ D.loeschen d `catch` \ any -> return ()
		  plain $ "geloescht: " ++ show d

             _ -> do
                  mtriple <- show_previous ( Edit == act ) 
                                 vnr mks stud auf sauf
		  case mtriple of
		      Nothing -> return ()
		      Just ( inst, inp, res, com ) -> do
                           [ stud ] <- io $ S.get_snr $ SA.snr sauf
			   -- das mu� auch nach Einsendeschlu� gehen,
			   -- weil es der Tutor ausf�hrt
			   Inter.Common.punkte tutor stud auf ( inst, inp, res, com )
	 mzero

    let manr = fmap A.anr mauf
    
    when ( Delete == action ) $ do
        Just anr <- return manr
	wirk <- submit "wirklich l�schen?"
	when wirk $ do
            io $ A.delete anr
            plain $ unwords [ "Aufgabe", show anr, "gel�scht." ]
	mzero

    ( mk, type_click ) <- find_mk tmk tutor mauf

    auf' <- if tutor 
            then do
		 edit_aufgabe mks mk mauf vnr manr type_click
	    else -- kein tutor 
                case mauf of
		  Nothing -> do 
                      -- kommt eigentlich nicht vor?
		      plain "keine Aufgabe gew�hlt"
		      mzero
		  Just auf -> do
                      return auf
    stud' <- get_stud tutor stud

    case action of
        Config -> do
	    solution vnr manr stud' mk auf' 
	    return ()
        Solve -> do
            ( minst, cs, res, com ) <- solution vnr manr stud' mk auf' 
	    Inter.Common.punkte False stud' auf' ( minst, cs, Just res, com )
	Edit | tutor -> do
	    find_previous True  vnr mks stud' auf'
            return ()
	View -> do
	    find_previous False vnr mks stud' auf'
            return ()

    hr
    con <- io $ Inter.Motd.contents
    html con

    return ()

-------------------------------------------------------------------------

-- | bestimme aufgaben-typ (maker)
-- f�r tutor: w�hlbar
-- f�r student: fixiert (ohne dialog)
find_mk tmk tutor mauf = do
    let pre_mk = fmap (toString . A.typ) mauf
    if tutor 
            then do
		 hr
		 h3 "Parameter dieser Aufgabe:"
		 open btable -- will be closed in edit_aufgabe (tutor branch)
		 -- selector_submit_click "Typ" pre_mk opts
                 it <- tree_choice pre_mk $ fmap ( \ n -> case n of
                                 Right mk -> Right ( show mk, mk )
                                 Left heading -> Left $ heading ++ " .."
                                    ) tmk
                 return ( it, True ) -- FIXME
            else do
		 Just pre <- return $ pre_mk
                 let mks = do 
                        Right mk <- flatten tmk
                        return ( show mk, mk )
		 Just it  <- return $ lookup pre mks
		 return ( it, False )

-- | �ndere aufgaben-konfiguration (nur f�r tutor)
edit_aufgabe mks mk mauf vnr manr type_click = do
    case mk of 
        Make doc ( fun :: conf -> Var p i b ) verify ex -> do

            let t = fromCGI $ show mk

            others <- io $ A.get_typed t

            ( name :: Name ) <- fmap fromCGI 
		     $ defaulted_textfield "Name" 
		     $ case mauf of Nothing -> foldl1 (++) 
					       [ toString t , "-"
					       , show $ succ $ length others 
					       ]
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

            (dflt_von,dflt_bis) <- io defaults

            ( von :: Time ) <- fmap fromCGI 
		    $ defaulted_textfield "von" 
		    $ case mauf of Nothing -> dflt_von
				   Just auf -> toString $ A.von auf
            ( bis ::Time ) <- fmap fromCGI
		    $ defaulted_textfield "bis" 
		    $ case mauf of Nothing -> dflt_bis
				   Just auf -> toString $ A.bis auf

            moth <- 
                click_choice_with_default 0 "importiere Konfiguration" 
                     $  ("(default)", mauf) : do
                           oth <- others
                           return ( toString $ A.name oth , Just oth )
            let ( mproto, type_changed ) = case moth of
                   Just oth -> ( moth, False )
                   Nothing  -> ( mauf, type_click )

            -- nimm default-config, falls type change 
            -- FIXME: ist das sinnvoll bei import?
            conf <- editor_submit "Konfiguration" 
		    $ case mproto of 
			  Just auf | not type_changed   -> 
			       case parse ( parsec_wrapper 0 ) "input"
				          ( toString $ A.config auf ) of
				        Right ( x, rest ) -> x
					Left err -> ex
			  _ -> ex

	    close -- table

	    -- check configuration

	    br
	    
	    Just _ <- embed $ do
		 inform $ text "verifiziere die Konfiguration:"
	         verify conf
		 inform $ text "OK"
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
			       , A.timeStatus = Control.Types.Early -- ist egal
			       }
            when up $ io $ A.put manr auf'
            return auf'

-- | matrikelnummer zum aufgabenl�sen:
-- tutor bekommt eine gew�rfelt (und kann neu w�rfeln)
-- student bekommt genau seine eigene
get_stud tutor stud = 
    if tutor 
       then do
         hr
	 m0 <- io $ randomRIO (0, 999999 :: Int) 
	 -- neu w�rfeln nur bei �nderungen oberhalb von hier
	 plain "eine gew�rfelte Matrikelnummer:"
	 mat <- with ( show m0 ) $ textfield ( show m0 )
         -- falls tutor, dann geht es hier nur um die matrikelnr
	 return $ stud { S.mnr = fromCGI mat
		       , S.snr = error "gibt es nicht"
		       }
       else do
	 return stud


find_previous edit vnr mks stud auf = do

    -- kann sein, da� S.anr  error  ergibt (f�r tutor)
    sas <- io $ SA.get_snr_anr (S.snr stud) (A.anr auf) 
                   `Control.Exception.catch` \ any -> return []
    case sas of
        [ sa ] -> do
	    show_previous edit vnr mks stud auf sa 
        _ -> return Nothing

fix_input vnr mks stud auf sa = case  SA.input sa of
   Just file -> return $ Just file
   Nothing -> io $ do
       -- fix location of previous einsendung
       let p = mkpar stud auf
           d = Inter.Store.location Inter.Store.Input 
                    p "latest" False
       file <- D.home d
       ex <- System.Directory.doesFileExist file
       let inf = fromCGI file
       if ex 
           then do
                -- nur infile-location einschreiben
                Control.Punkt.bepunkteStudentDB 
                         (P.ident p) (P.anr p) 
                         Nothing
                         Nothing (P.highscore p) 
                         ( Just inf )
                         Nothing
                return $ Just inf
           else return $ Nothing

fix_instant vnr mks stud auf sa = case SA.instant sa of
   Just file -> return $ Just file
   Nothing -> 
       -- transitional:
       -- (try to) re-generate previous instance
       let mmk = lookup ( toString $ A.typ auf ) 
                $ do mk <- mks ; return ( show mk, mk )
       in case mmk of
            Nothing -> do
                plain "Aufgabenstellung nicht auffindbar"
                return Nothing
            Just ( Make doc fun veri ex ) -> do
                ( _, _, com ) <- make_instant 
		    vnr ( Just $ A.anr auf ) stud fun auf
                let p = mkpar stud auf
                    d = Inter.Store.location Inter.Store.Instant
                           p "latest" False
                file <- io $ D.schreiben d $ show com
                let inst = fromCGI file
                io $ Control.Punkt.bepunkteStudentDB 
                         (P.ident p) (P.anr p) 
                         ( Just inst )
                         Nothing (P.highscore p) 
                         Nothing 
                         Nothing
                return $ Just inst

-- | TODO: possibly with edit (for tutor)
show_previous edit vnr mks stud auf sa0 = do

    inf <- fix_input vnr mks stud auf sa0
    ins <- fix_instant vnr mks stud auf sa0
    let sa = sa0 { SA.input = inf, SA.instant = ins }

    hr ;  h3 "Vorige Einsendung und Bewertung zu dieser Aufgabe"
    -- pre $ show sa
    br ; plain "Aufgabenstellung:"
    case SA.instant sa of
        Just file -> do
            cs <- io $ logged "Super.view" 
    	         $ readFile $ toString file
    	    html $ Text.Html.primHtml cs
        Nothing -> do
	    plain "(keine Aufgabe)"
    br ; plain "Einsendung:"
    case SA.input sa of
        Just file -> do
            cs <- io $ logged "Super.view" 
    	         $ readFile $ toString file
    	    pre cs
        Nothing -> do
	    plain "(keine Einsendung)"
    br ; plain "Bewertung:"
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
             html $ Text.Html.primHtml h
             blank -- ??
	     return Nothing
         True  -> do
               mcom <- textarea h
               let com = fromMaybe h mcom   
               open table
               mgrade <- click_choice0  "Grade" $
			[ ("(none)", Nothing )
			, ("Pending", Just Pending)
			, ("No", Just No) 
                        ] ++ do
                           p <- [ 1 .. 10 ]
                           let w = Okay { Control.Types.punkte = p, size = 1 }
			   return (show w, Just $ w)
			
               close -- table
	       return $ Just ( Nothing
			     , Nothing 
			     , mgrade
			     , case mgrade of 
			           Just x | x /= Pending -> 
			                 Just $ Text.Html.primHtml com 
			           _ -> Nothing
			     )


make_instant vnr manr stud fun auf = do
    -- let conf = read $ toString $ A.config auf
    conf <- 
        case parse ( parsec_wrapper 0 ) "input" $ toString $ A.config auf of
	     Right ( x, rest ) -> return x
	     Left err -> do
		  plain "should not happen: parse error in A.config"
		  pre $ show err
		  pre $ show $ A.config auf
		  mzero
    io $ make_instant_common vnr manr stud $ fun conf


data Method = Textarea | Upload
    deriving ( Eq, Show, Typeable )

-- | eingabe und bewertung der l�sung
-- f�r tutor zum ausprobieren
-- f�r student echt
solution vnr manr stud 
        ( Make doc ( fun :: conf -> Var p i b ) verify ex ) auf = do

    ( p, i, icom ) <- make_instant vnr manr stud fun auf

    let past = mkpar stud auf

    let ini  = initial p i
    br
    parameter_table auf

    h3 "Aufgabenstellung"
    html icom

    when ( not $ A.current auf ) vorbei

    ---------------------------------------------------------
    html $ Text.Html.anchor Text.Html.! [ Text.Html.name "hotspot" ]
         Text.Html.<< ""

    h3 "Neue Einsendung"

    open table
    method <- click_choice_with_default 0 "Eingabe-Methode"
       [ ( "Textfeld", Textarea ), ( "Datei-Upload", Upload ) ]
    close

    mcs <- case method of
        Textarea -> do
	    ex    <- submit "Beispiel laden"
	    prev  <- submit "vorige Einsendung laden"
	    -- esub  <- submit "Textfeld absenden"
	    br
	    when ( ex || prev ) blank

            let b0 = render $ toDoc ini 
	    def <- io $ if prev 
	      then Inter.Store.latest Inter.Store.Input past
                      `Control.Exception.catch` \ _ -> return b0
	      else return b0
	    open table
	    open row
            let helper :: Text.Html.Html
                helper = Autolib.Output.render 
                 $ Autolib.Output.Beside
                      ( Autolib.Output.Text "ein Ausdruck vom Typ" )
                      ( help ini )
            html helper
	    close -- row
	    open row
            sol <- textarea def
	    esub  <- submit "Textfeld absenden"
	    close -- row
	    close -- table
            return sol

	Upload -> do
            plain "Datei ausw�hlen:"
            up <- file undefined
	    fsub  <- submit "Datei absenden"
            when ( not fsub ) $ mzero -- break
	    return up

    Just cs <- return mcs
    hr ; h3 "Neue Bewertung"
    (res, com :: Text.Html.Html) <- io $ run $ evaluate p i cs
    html com
    return ( Just icom, Just cs, fromMaybe No res, Just com )

parameter_table auf = do
    h3 $ unwords [ "Aufgabe", toString $ A.name auf ]
    above ( plain "Hinweise" )
	            ( pre $ toString $ A.remark auf )


------------------------------------------------------------------------

data Action = Solve  -- ^ neue L�sung bearbeiten
	    | View -- ^ alte L�sung + Bewertung ansehen
	    | Edit -- ^ alte L�sung + Bewertung �ndern
	    | Rescore [ ( Wert, SA.Stud_Aufg, S.Student ) ] 
            | Clear_Cache
	    | Statistics 
	    | Config
            | Delete 
            | Add
     deriving ( Show, Eq, Typeable )

-- data Display = Current | Old 
--     deriving ( Show, Eq, Typeable )

-- | f�r Student: statistik aller seiner Aufgaben anzeigen, 
-- f�r Tutor: kann Aufgabenl�sung sehen und (nach-)korrigieren
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
	    let stat = if A.current auf then Current else Late
	    return ( auf, stat, okno )

    open btable
    disp <- click_choice_with_default 0 "Aufgaben anzeigen:"
	      [ ( "nur aktuelle", [ Current ] )
	      , ( "alle"   , [ Late, Current , Early ] )
	      ]
    close -- btable
    br

    -- daten anzeigen
    let dat = do
	    begin -- mutex
            open btable
            open row
            plain "Aufgabe" ; plain "Status" ; plain "Highscore"
            plain "Bearbeitungszeit"
            plain "vorige Bewertung" ; plain "Gesamt-Wertungen"
            close -- row
            sequence_ $ do 
                ( auf, _ , ( ok, no, mres ) ) <- score
		guard $ A.timeStatus auf `elem` disp
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
                    farbe col $ case A.timeStatus auf of
                        Early -> "erst ab " ++ toString ( A.von auf )
                        Current -> "noch bis " ++ toString ( A.bis auf )
                        Late -> "vorbei seit " ++  toString ( A.bis auf )
                    farbe col $ case mres of
				  Just res -> show res
				  Nothing  -> ""
            	    farbe col $ show ( ok , no )
                    sequence_ $ do
		        ch <- case A.timeStatus auf of
			        _ | tutor -> [ View, Edit ]
			        Early     -> []
				_         -> [ Solve, View ]
                        return $ click ( show ch , ( ch, auf ))
            	    close -- row
            close -- table
            end -- mutex
    -- auswerten
    let goal = sum $ do 
            ( auf, _ , okno ) <- score
	    guard $ A.status auf == Mandatory
	    return ( 1 :: Int )
	done = sum $ do 
            ( auf, _ , (ok, no, mres) ) <- score
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
		-> Form IO ( Action, SA.Stud_Aufg, S.Student )
tutor_statistik vnr auf = do
    hr
    saufs <- io $ Control.Stud_Aufg.DB.get_anr $ A.anr auf

    h3 "Statistik f�r diese Aufgabe"


    open_btable_with_sorter 
	      [ "Matrikel", "Vorname" , "Name" , "Oks" , "Nos" , "Result" ]

    begin -- mutex


    -- erstmal die, die wirklich was eingesandt haben
    rscores1 <- sequence $ do
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
	    click ( "View", ( View, sauf, stud ))
	    click ( "Edit",  ( Edit, sauf, stud ))
	    click ( "Clear_Cache",  ( Clear_Cache, sauf, stud ))
            rs <- radio_score sauf stud
            close -- row
	    return rs    

    -- dann die, die noch gar nichts geschickt haben
    all_studs <- io $ Control.Vorlesung.DB.steilnehmer vnr
    let done = mkSet $ map SA.snr saufs
    rscores2 <- sequence $ do
          stud <- all_studs
          guard $ not $ S.snr stud `elementOf` done
          return $ do
              open row
	      plain $ toString $ S.mnr stud
	      plain $ toString $ S.vorname stud
	      plain $ toString $ S.name stud
              plain $ "--"
	      plain $ "--"
	      plain $ "--"
              sauf <- io $ Control.Stud_Aufg.DB.put_blank 
                              (S.snr stud) (A.anr auf)
	      click ( "Edit",  ( Edit, sauf, stud ))
              rs <- radio_score sauf stud
              close -- row
	      return rs

    close -- btable
    click ( "Edits ausf�hren"
	  , ( Rescore ( concat $ rscores1 ++ rscores2 ) 
	    , error "sa" :: SA.Stud_Aufg
	    , error "s" :: S.Student
	    )
	  )
    end -- mutex

-- | input widget for a score
radio_score sauf stud = do
    p <- radiogroup "keep" 
            $ ( "keep", [] )
	    : ( "No"  , [ ( No, sauf, stud ) ] )
	    : do p <- [ 1 .. 5 ] 
		 return ( show p
			, [ ( Okay { Control.Types.punkte = p, size = 1 } 
			    , sauf, stud
			    )
			  ] 
			)
    return $ concat $ maybeToList p

-----------------------------------------------------------------------------

footer scores = do
    hr ; h3 "Informationen zum autotool"
    let entry name url = O.Named_Link name url
    embed $ output
          $ foldr1 O.Beside
	      [ entry "autotool home" 
		      "http://dfa.imn.htwk-leipzig.de/auto/"
              , O.Text "/"
	      , entry "bugs (ansehen und melden)" 
		      "http://dfa.imn.htwk-leipzig.de/bugzilla/buglist.cgi?component=autotool&bug_status=NEW&bug_status=ASSIGNED&bug_status=REOPENED"
              , O.Text "/"
	      , entry "highscores" scores
	      ]
    hr
