-- |  Statistik

module Main where

import Inter.CGI
import Inter.Login
import Control.Types

import qualified Control.Aufgabe.Typ as A
import qualified Control.Aufgabe.DB

import qualified Control.Vorlesung.DB

import Control.Stud_Aufg.Typ
import Control.Stud_Aufg.DB
import Control.Student.DB
import Control.Student.CGI

import Autolib.ToDoc
import Autolib.FiniteMap
import Autolib.Set
import Autolib.Util.Sort

import Control.Monad

main :: IO ()
main = Inter.CGI.execute "Statistik.cgi" $ iface 

iface :: Form IO ()
iface = do

    h3 "autotool: Statistiken"
    open btable
    svt @ ( stud, vnr, tutor ) <- Inter.Login.form
    close -- btable

    guard $ tutor

    open btable
    ( action, _ ) <- selector_submit_click "zeige" Nothing
           [ ("Resultate (mandatory)", resultate vnr True ) 
           , ("Resultate (alle)"     , resultate vnr False) 
	   , ("Studenten", studenten vnr)
	   ]
    action

--------------------------------------------------------------------------

studenten vnr = do
    t <- io $ Control.Vorlesung.DB.teilnehmer vnr
    ( mnr, _ ) <- selector_submit_click "bearbeite" Nothing $ do
        (mnr, v, n) <- sortBy (\(m,v,n) -> n) $ map snd t
        let s = unwords [ toString mnr , toString v, toString n ]
        return (s, mnr)
    close -- btable
    [ stud ] <- io $ Control.Student.DB.get_mnr mnr
    Control.Student.CGI.edit stud
    
--------------------------------------------------------------------------

resultate vnr only_mandatory = do
    close -- btable

    t <- io $ Control.Vorlesung.DB.teilnehmer vnr
    let fmt = listToFM t -- snr to (mnr, vorname,  name)

    aufs0 <- io $ Control.Aufgabe.DB.get ( Just vnr )
    let aufs = do 
           auf <- aufs0
	   guard $ only_mandatory <= ( A.status auf == Mandatory )
	   return auf

    -- anr to name
    let fma = listToFM $ do 
	   auf <- aufs
	   return ( A.anr auf, A.name auf )

    items <- sequence $ do
        auf <- aufs
	return $ io $ Control.Stud_Aufg.DB.get_anr $ A.anr auf
   
    let stud_aufg = listToFM $ do
          its <- items ; it <- its
          return ( (snr it, anr it) , ( ok it, no it ))
    let keys = mkSet $ keysFM stud_aufg
	snrs = smap fst keys
	anrs = smap snd keys

    h3 "Einsendungen (Ok/No)"
    open sbtable
    open row
    plain "MNr" ; plain "Vorname" ; plain "Name"
    sequence_ $ do
        anr <- setToList anrs
	return $ plain $ case lookupFM fma anr of
	    Just name -> ( if only_mandatory then take 3 else id )
			 $ toString name
	    Nothing   -> show anr
    plain "total"
    close -- row
    sequence_ $ do
        snr <- setToList $ snrs
	let (mnr, vorname, name) = case lookupFM fmt snr of
	       Just (mnr, vorname, name) 
		   -> (toString mnr, toString vorname, toString name)
	       Nothing -> ( "??", "??", "??" )
	return $ do
	    open row 
	    plain mnr ; plain vorname ; plain name
	    nums <- sequence $ do
	        anr <- setToList $ anrs
                let result = lookupFM stud_aufg (snr, anr) 
		return $ do
		     plain $ case result of
		         Just ( Oks o, Nos n ) -> 
			     if only_mandatory 
			     then show o else show (o, n)
		         Nothing -> "-"
		     return $ case result of
			 Just ( Oks o, _ ) | o > 0 -> 1
			 _                         -> 0

            plain $ show $ sum nums
            close -- row

        
    close -- btable




