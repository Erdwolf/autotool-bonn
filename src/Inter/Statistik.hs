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

import Autolib.ToDoc
import Autolib.FiniteMap
import Autolib.Set

main :: IO ()
main = Inter.CGI.execute "Statistik.cgi" $ iface 

iface :: Form IO ()
iface = do

    h3 "autotool: Statistiken"
    open btable
    ( stud, vnr, tutor ) <- Inter.Login.form
    close -- btable

    t <- io $ Control.Vorlesung.DB.teilnehmer vnr
    let fmt = listToFM t -- snr to (mnr, name)

    aufs <- io $ Control.Aufgabe.DB.get ( Just vnr ) False
    -- anr to name
    let fma = listToFM $ do auf <- aufs ; return ( A.anr auf, A.name auf )

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
    open btable
    open row
    plain "MNr" ; plain "Name"
    sequence_ $ do
        anr <- setToList anrs
	return $ plain $ case lookupFM fma anr of
	    Just name -> toString name
	    Nothing   -> show anr
    close -- row
    sequence_ $ do
        snr <- setToList $ snrs
	let (mnr, name) = case lookupFM fmt snr of
	       Just (mnr, name) -> (toString mnr, toString name)
	       Nothing -> ("??", "??")
	return $ do
	    open row 
	    plain mnr ; plain name
	    sequence_ $ do
	        anr <- setToList $ anrs
		return $ do
		     plain $ case lookupFM stud_aufg (snr, anr) of
		         Just ( Oks o, Nos n ) -> show (o, n)
		         Nothing -> "-"
            close -- row
    close -- btable




