-- | edit the problem configuration
-- and check a sample solution

--  $Id$

module Main where

import Inter.CGI
import Control.Types ( toString, fromCGI, Name, Remark, HiLo, Time )


import qualified Inter.Collector


import Challenger.Partial
import Inter.Types
import Inter.Click

import Inter.Make 
import Inter.Evaluate

import qualified Control.Aufgabe as A
import qualified Control.Vorlesung as V


import Autolib.Reporter.Type
import Autolib.ToDoc
import Autolib.Reader


import Random
import Data.Typeable
import Data.Maybe
import Data.List
import Control.Monad


main :: IO ()
main = Inter.CGI.execute "Super.cgi" 
     $ iface Inter.Collector.makers

iface :: [ Make ] -> Form IO ()
iface mks = do

    -- TODO: check tutor-login bzw. stud-login
    -- unterschiede: tutor darf "alles",
    -- student darf kein aufgaben ändern und nur aktuelle aufgaben sehen
    vors <- io $ V.get
    vnr <- selector_submit "vnr" "wähle Vorlesung" 0 $ do
        vor <- vors
        return ( show $ V.name vor , V.vnr vor )

    -- FIXME: nicht alle aufgaben aus DB holen, 
    -- sondern nur die mit passender VNr
    aufs <- io $ A.get  
    br
    mauf <- selector_submit "anr" "bearbeite Aufgabe" 0 
    -- TODO: implementiere "delete aufgabe"
      $ ( "(neue Aufgabe)", Nothing ) : do
        auf <- aufs
        guard $ A.vnr auf == vnr
        return ( show $ A.name auf , Just $ auf )
    let manr = fmap A.anr mauf

    pre $ "Vorlesung gewählt : " ++ show vnr
    pre $ "Aufgabe gewählt : " ++ show manr

    hr ---------------------------------------------------------

    let pre_mk = fmap (toString . A.typ) mauf
    mk <- selector_submit' "mk" "wähle Aufgabentyp" pre_mk $ do
        mk <- mks
	return ( show mk, mk )

    case mk of 
        Make doc ( fun :: conf -> Var p i b ) ex -> do
            conf <- editor_submit "conf" "Konfiguration" ( ex :: conf )
            let var = fun conf
                p = problem var
            br
	    h3 "Zur Übernahme in die Datenbank:"
            ( name :: Name ) <- fmap fromCGI 
		     $ defaulted_textfield "name" "noch kein Name"
            br -- TODO: überschrift "remark"
	    ( remark :: Remark ) <- fmap fromCGI 
		    $ defaulted_textarea "rem" "noch keine Hinweise"
            br
	    ( hilo :: HiLo ) <- defaulted_selector "high" "Highscore" 0 $ do
		     ( x :: HiLo ) <- [ minBound .. maxBound ]
                     return ( show x, x )
            br -- FIXME: get now() from DB
            ( von :: Time ) <- fmap fromCGI 
		    $ defaulted_textfield "von" ( "2004-10-15 11:16:08" )
	    br
            ( bis ::Time ) <- fmap fromCGI
		    $ defaulted_textfield "bis" ( "2004-10-15 11:16:08" )
	    br
	    up <- submit "update" "update data base"
            when up $ io $ A.put manr $ A.Aufgabe 
		               { A.anr = error "Super.anr" -- intentionally
			       , A.vnr = vnr
			       , A.name = name
			       , A.typ = fromCGI $ show mk
			       , A.config = fromCGI $ show conf
			       , A.remark = remark
			       , A.highscore = hilo
			       , A.von = von
			       , A.bis = bis
			       }

            hr ---------------------------------------------------------

            m0 <- io $ randomRIO (0, 999999 :: Int) 
            -- gewürfelte matrikelnummer
            -- neu würfeln nur bei änderungen oberhalb von hier
            mat <- with ( show m0 ) $ textfield "mat" ( show m0 )
            k <- io $ key var mat 
            g <- io $ gen var k
            let ( Just i , com :: Doc ) = export g
                desc = describe (problem var) i
                ini  = initial  (problem var) i
            h3 "Aufgabenstellung"
            pre $ show desc
 
            hr ---------------------------------------------------------

            b <- editor_submit "b" "Lösung" ini
     	    let (res, com :: Doc) = export $ evaluate' p i b
            pre $ show com
	    -- TODO: bewertung in DB (für Stud-Variante)

--------------------------------------------------------------------------

-- TODO: move to separate module (chop Control.CGI into pieces as well)

defaulted_selector tag title def opts = do
    ms <- selector tag title def opts
    return $ fromMaybe ( snd $ opts !! def ) ms

defaulted_textfield tag def = do
    ms <- textfield tag def
    return $ fromMaybe def ms

defaulted_textarea tag def = do
    ms <- textarea tag def
    return $ fromMaybe def ms

selector_submit tag title def opts = do
    mopt <- selector ("L" ++ tag) title def opts
    sopt <- submit   ("S" ++ tag) "submit"
    case mopt of
	 Nothing -> mzero
	 Just opt -> do
	      when sopt blank
	      return opt

selector_submit' :: Monad m
	         => Tag 
		 -> String 
		 -> Maybe String -- ^ possible default
		 -> [(String, a) ] 
		 -> Form m a
selector_submit' tag title def opts = do
    mopt <- selector' ("L" ++ tag) title (fromMaybe "XX" def) opts
    sopt <- submit   ("S" ++ tag) "submit"
    case mopt of
	 Nothing -> mzero
	 Just opt -> do
	      when sopt blank
	      return opt

editor_submit :: ( ToDoc a, Reader a, Monad m )
	      => Tag -> String -> a 
	      -> Form m a
editor_submit tag title ex = do
    mconf <- editor ("E" ++ tag) title ex
    sconf <- submit ("S" ++ tag) "submit"
    case mconf of
	 Nothing -> mzero
	 Just conf -> do
             when sconf blank
	     return conf
