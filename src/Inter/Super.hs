-- | edit the problem configuration
-- and check a sample solution

--  $Id$

module Main where

import Inter.CGI
import Inter.Evaluate
import Inter.Make 
import Inter.Bank
import Inter.Store 
import qualified Inter.Param as P

import Control.Types ( toString, fromCGI, Name, Remark, HiLo (..), Time )


import qualified Inter.Collector


import Challenger.Partial
import Inter.Types

import Control.Student.CGI
import Control.Vorlesung.DB

import qualified Control.Aufgabe as A
import qualified Control.Student.Type as S
import qualified Control.Vorlesung as V

import Autolib.Reporter.Type
import Autolib.ToDoc
import Autolib.Reader

import Random
import Data.Typeable
import Data.Maybe
import Data.List
import Control.Monad
import qualified Control.Exception

import Text.Html ( Html )

main :: IO ()
main = Inter.CGI.execute "Super.cgi" 
     $ iface Inter.Collector.makers

iface :: [ Make ] -> Form IO ()
iface mks = do

    stud <- Control.Student.CGI.login
    let snr = S.snr stud
    hr
    open btable
 
    tvors <- io $ V.get_tutored snr
    -- unterschiede: tutor darf "alles",
    -- student darf keine aufgaben ändern und nur aktuelle aufgaben sehen
    let tutor = not $ null tvors
    vors <- if tutor 
            then return tvors
	    else io $ V.get_attended snr

    vnr <- selector_submit "vnr" "Vorlesung" 0 $ do
        vor <- vors
        return ( show $ V.name vor , V.vnr vor )

    aufs <- io $ A.get ( Just vnr ) ( not tutor ) -- student darf nur aktuelle
    let opts = do
                     auf <- aufs
                     return ( show $ A.name auf , Just $ auf )
    ( mauf, del ) <- 
        if tutor
	   then selector_edit_delete "anr" "Aufgabe" 0 
                   $ ( "(neue Aufgabe)", Nothing ) : opts
           else do 
		( mauf, _ ) <- selector_submit_click "anr" "Aufgabe" Nothing $ opts
                return ( mauf, False )
    let manr = fmap A.anr mauf
    close -- table
    
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
    ( cs, res ) <- solution stud' mk auf' 
    -- bewertung in DB (für Stud-Variante)
    when ( not tutor ) $ punkte stud' auf' ( cs, res )
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
            -- FIXME: get now() from DB
            ( von :: Time ) <- fmap fromCGI 
		    $ defaulted_textfield "von" 
		    $ case mauf of Nothing -> "2004-10-15 11:16:08"
				   Just auf -> toString $ A.von auf
            ( bis ::Time ) <- fmap fromCGI
		    $ defaulted_textfield "bis" 
		    $ case mauf of Nothing -> "2004-10-15 11:16:08"
				   Just auf -> toString $ A.bis auf
	    close -- table

            -- nimm default-config, falls type change
            conf <- editor_submit "conf" "Konfiguration" 
		    $ case mauf of 
			  Just auf | not type_click  -> 
				 read $ toString $ A.config auf
			  _ -> ex :: conf
				   
            br
	    up <- submit "update" "update data base"
            let auf' = A.Aufgabe 
		               { A.anr = error "Super.anr" -- intentionally
			       , A.vnr = vnr
			       , A.name = name
			       , A.typ = fromCGI $ show mk
			       , A.config = fromCGI $ show conf
			       , A.remark = remark
			       , A.highscore = fromMaybe Keine mhilo
			       , A.von = von
			       , A.bis = bis
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
         -- falls tutor, dann geht es hier nur im die matrikelnr
	 return $ stud { S.mnr = fromCGI mat
		       , S.snr = error "gibt es nicht"
		       }
       else do
	 return stud

-- | eingabe und bewertung der lösung
-- für tutor zum ausprobieren
-- für student echt
solution stud ( Make doc ( fun :: conf -> Var p i b ) ex ) auf = do

            let conf = read $ toString $ A.config auf
                var = fun  conf
                p = problem var
            let mat = S.mnr stud
            k <- io $ key var $ toString mat 
            g <- io $ gen var k
            let ( Just i  , com :: Doc ) = export g
                desc = describe (problem var) i
                ini  = initial  (problem var) i
            h3 "Aufgabenstellung"
            pre $ show desc
            br
	    plain "Hinweise"
	    pre $ toString $ A.remark auf

            hr ---------------------------------------------------------
	    h3 "Lösung"

            sub <- submit "subsol" "submit"
            prev <- submit "subprev" "previous"
            ex <- submit "subex" "example"
	    br
            when ex blank

            let b0 = render $ toDoc ini 
            def <- io $ if prev 
		   then Inter.Store.latest (mkpar stud auf)
                              `Control.Exception.catch` \ _ -> return b0
		   else return b0
		

            Just cs <- textarea "sol" def

     	    let (res, com :: Html) = export $ evaluate p i cs
            html com
	    return ( cs, res )

-- | erreichte punkte in datenbank schreiben 
-- und lösung abspeichern
punkte stud auf ( cs, res ) = do
     hr
     let p = ( mkpar stud auf )  { P.input = cs }
     msg <- io $ bank p res
     plain $ "Eintrag ins Logfile:"
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

--------------------------------------------------------------------------

-- TODO: move to separate module (chop Control.CGI into pieces as well)

defaulted_selector tag def opts = do
    open row
    plain tag
    ms <- selector tag def opts
    close
    return $ fromMaybe ( snd $ opts !! def ) ms

defaulted_textfield tag def = do
    open row
    plain tag
    ms <- textfield tag def
    close -- row
    return $ fromMaybe def ms

defaulted_textarea tag def = do
    open row
    plain tag
    ms <- textarea tag def
    close -- row
    return $ fromMaybe def ms

selector_submit tag title def opts = do
    open row
    plain title
    mopt <- selector ("L" ++ tag) def opts
    sopt <- submit   ("S" ++ tag) "submit"
    close -- row
    case mopt of
	 Nothing -> case opts of
              [( name, opt) ] -> return opt
	      _       -> mzero
	 Just opt -> do
	      when sopt blank
	      return opt

selector_edit_delete tag title def opts = do
    open row
    plain title
    mopt <- selector ("L" ++ tag) def opts
    sopt <- submit   ("S" ++ tag) "edit"
    dopt <- submit   ("D" ++ tag) "delete"
    close -- row
    case mopt of
	 Nothing -> mzero
	 Just opt -> do
	      when sopt blank
	      return ( opt, dopt )


-- | if default is Nothing, then stop here, else continue with default
selector_submit_click :: Monad m
	         => Tag 
		 -> String 
		 -> Maybe String -- ^ possible default
		 -> [(String, a) ] 
		 -> Form m (a, Bool)
selector_submit_click tag title def opts = do
    open row
    plain title
    mopt <- selector' ("L" ++ tag) (fromMaybe "XX" def) opts
    sopt <- submit   ("S" ++ tag) "submit"
    close
    case mopt of
	 Nothing -> case def of
	      Nothing -> mzero
	      Just d -> case lookup d opts of
		   Nothing -> mzero
                   Just opt -> return ( opt, False )
	 Just opt -> do
	      when sopt blank
	      return ( opt, sopt )

selector_submit' tag title def opts = do
    ( opt, sopt ) <- selector_submit_click tag title def opts
    return opt

editor_submit :: ( ToDoc a, Reader a, Monad m )
	      => Tag -- ^ tag
	      -> String -- ^ title
	      -> a -- ^ default
	      -> Form m a
editor_submit tag title ex = do
    open row
    plain title
    sconf <- submit ("S" ++ tag) "submit"
    mconf <- editor ("E" ++ tag) ex
    close -- row
    case mconf of
	 Nothing -> do
	     -- mzero
	     return ex
	 Just conf -> do
             when sconf blank
	     return conf
