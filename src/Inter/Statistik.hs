-- |  Statistik

module Inter.Statistik where

import Inter.CGI
import Inter.Login
import Control.Types

import qualified Control.Aufgabe.Typ as A
import qualified Control.Aufgabe.DB

import qualified Control.Vorlesung.DB
import qualified Control.Vorlesung.Typ as V

import Control.Stud_Aufg.Typ
import Control.Stud_Aufg.DB
import Control.Student.DB
import Control.Student.CGI

import qualified Control.Student as S
import Control.Stud_Aufg as SA

import Autolib.ToDoc
import Autolib.FiniteMap
import Autolib.Set
import Autolib.Util.Sort

import Control.Monad

-- main :: IO ()
-- main = Inter.CGI.execute "Statistik.cgi" $ iface 

main svt @ ( stud, vor, tutor, attends ) = do

    h3 "autotool: Statistiken"

    guard $ tutor

    open btable
    ( action, _ ) <- selector_submit_click "zeige" Nothing
           [ ("Resultate (mandatory)", resultate vor True ) 
           , ("Resultate (alle)"     , resultate vor False) 
	   , ("Studenten", studenten vor)
	   ]
    action

--------------------------------------------------------------------------

studenten vor = do
    studs <- io $ Control.Vorlesung.DB.steilnehmer $ V.vnr vor
    ( stud , _ ) <- selector_submit_click "bearbeite" Nothing $ do
        stud <- sortBy S.mnr studs
        let s = unwords [ toString $ S.mnr stud
                        , toString $ S.vorname stud
                        , toString $ S.name stud
                        ]
        return (s, stud)
    close -- btable
    Control.Student.CGI.edit stud
    
--------------------------------------------------------------------------

resultate vor only_mandatory = do
    let vnr = V.vnr vor
    close -- btable

    studs <- io $ Control.Vorlesung.DB.steilnehmer vnr
    -- let fmt = listToFM t -- snr to (mnr, vorname,  name)

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
   
    let stud_aufg :: FiniteMap ( SNr, ANr ) ( Oks, Nos )
        stud_aufg = listToFM $ do
          its <- items ; it <- its
          return ( (SA.snr it, SA.anr it) , ( SA.ok it, SA.no it ))
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
        stud <- studs
	let mnr = S.mnr stud
            vorname = S.vorname stud
            name = S.name stud
	return $ do
	    open row 
	    plain $ toString mnr
            plain $ toString vorname
            plain $ toString name
	    nums <- sequence $ do
	        anr <- setToList $ anrs
                let result = lookupFM stud_aufg (S.snr stud, anr) 
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




