-- | edit the problem configuration
-- and check a sample solution

-- TODO: hack this module into pieces

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
import qualified System.Directory
import Data.Typeable
import Data.Maybe
import Data.Tree
import Data.List ( partition )
import Control.Monad
import qualified Control.Exception

import qualified Text.XHtml

import Inter.DateTime ( defaults )
import Inter.Tutor
import Inter.Student

main :: IO ()
main = Gateway.CGI.execute ( Local.cgi_name ++ "#hotspot" ) $ do
   wrap $ iface $ Inter.Collector.tmakers
   scores <- scores_link
   footer scores


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
    -- für Student und Tutor gleicher Start

    svt @ ( stud, vor, tutor0, attends0 ) <- Inter.Login.form

    ( tutor, attends ) <- 
        if tutor0
        then do
            h3 "Sie sind Tutor für diese Vorlesung."
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

-- | Studenten behandeln, die in keiner Übungsgruppe sind
waisenkinder :: UNr -> Form IO ()
waisenkinder u = do
    h3 $ "Waisenkinder"
    plain $  "Studenten Ihrer Schule, die keine Übungsgruppe gewählt haben"
    studs <- io $ S.orphans $ u
    open btable
    Inter.Statistik.edit_studenten studs

-- | alle Übungen,
-- markiere besuchte Übungen
-- one-click für verlassen\/besuchen 
veranstaltungen :: ( S.Student , V.Vorlesung , Bool ) -> Form IO ()
veranstaltungen ( stud , vor, False ) = do
    h3 "Einschreibung"

    -- dieser student für diese Vorlesung
    ags <- io $ G.get_attended ( V.vnr vor ) ( S.snr stud )

    case ags of
        [] -> plain "Sie sind in keine Übungsgruppe eingeschrieben."
        _  -> show_gruppen "Sie sind eingeschrieben in Übungsgruppe:" ags

    -- alle Gruppen für diese Vorlesung
    gs <- io $ G.get_this $ V.vnr vor
    show_gruppen "alle Übungsgruppen zu dieser Vorlesung:" gs

    when ( Control.Types.Current /= V.einschreib vor ) $ do
         br
	 plain $ unlines [ "Das Ein/Ausschreiben ist"
			 , "nur von " ++ show ( V.einschreibVon vor )
			 , "bis " ++ show ( V.einschreibBis vor )
			 , "möglich."
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
    
    h3 "Übungsgruppen zu dieser Vorlesung:"
    gs <- io $ G.get_this $ V.vnr vor

    open btable
    act <- click_choice "Aktion:"
        [ ("anzeigen", View )
        , ("erzeugen", Add )
	, ("bearbeiten", Edit )
	, ("löschen", Delete )
	]
    close -- btable
    case act of
	 View -> do
             show_gruppen "Übungsgruppen zu dieser Vorlesung:" gs
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
	     click <- submit "wirklich löschen?"
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
			      , Just $ Text.XHtml.primHtml 
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
			   -- das muß auch nach Einsendeschluß gehen,
			   -- weil es der Tutor ausführt
			   Inter.Common.punkte tutor stud auf ( inst, inp, res, com )
	 mzero

    let manr = fmap A.anr mauf
    
    when ( Delete == action ) $ do
        Just anr <- return manr
	wirk <- submit "wirklich löschen?"
	when wirk $ do
            io $ A.delete anr
            plain $ unwords [ "Aufgabe", show anr, "gelöscht." ]
	mzero

    ( mk, type_click ) <- find_mk tmk tutor mauf

    auf' <- if tutor 
            then do
		 auf' <- edit_aufgabe mks mk mauf vnr manr type_click
	         up <- submit "update data base: aufgabe"
                 when up $ io $ A.put manr auf'
                 return auf'

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


find_previous edit vnr mks stud auf = do

    -- kann sein, daß S.anr  error  ergibt (für tutor)
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
            Just ( Make p0 doc fun veri ex ) -> do
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
    	    html $ Text.XHtml.primHtml cs
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
             html $ Text.XHtml.primHtml h
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
                           let w = Control.Types.ok p
			   return (show w, Just $ w)
			
               close -- table
	       return $ Just ( Nothing
			     , Nothing 
			     , mgrade
			     , case mgrade of 
			           Just x | x /= Pending -> 
			                 Just $ Text.XHtml.primHtml com 
			           _ -> Nothing
			     )



------------------------------------------------------------------------

data Action = Solve  -- ^ neue Lösung bearbeiten
	    | View -- ^ alte Lösung + Bewertung ansehen
	    | Edit -- ^ alte Lösung + Bewertung ändern
	    | Rescore [ ( Wert, SA.Stud_Aufg, S.Student ) ] 
            | Clear_Cache
	    | Statistics 
	    | Config
            | Delete 
            | Add
     deriving ( Show, Eq, Typeable )

-- data Display = Current | Old 
--     deriving ( Show, Eq, Typeable )

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

    h3 "Statistik für diese Aufgabe"


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
    click ( "Edits ausführen"
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
			, [ ( Control.Types.ok p
			    , sauf, stud
			    )
			  ] 
			)
    return $ concat $ maybeToList p

