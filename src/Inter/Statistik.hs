-- |  Statistik

module Inter.Statistik where

import Gateway.CGI
import Inter.Login
import Control.Types

import qualified Control.Aufgabe.Typ as A
import qualified Control.Aufgabe.DB

import qualified Control.Vorlesung.DB
import qualified Control.Vorlesung.Typ as V

import qualified Control.Gruppe.DB
import qualified Control.Gruppe.Typ as G

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
import qualified Data.List

-- main :: IO ()
-- main = Inter.CGI.execute "Statistik.cgi" $ iface 

data Choice = None | All | Mandat deriving ( Eq, Ord, Show )

main svt @ ( stud, vor, status, attends ) = do

    h3 "autotool: Statistiken"

    guard $ status >= Tutor

    open btable
    action <- click_choice "zeige" 
           [ ("Resultate (mandatory)", resultate vor Mandat ) 
           , ("Resultate (alle)"     , resultate vor All ) 
	   , ("Studenten anzeigen", resultate vor None )
	   , ("einen Studenten bearbeiten", student_bearbeiten vor)
	   ]
    action

--------------------------------------------------------------------------

student_bearbeiten vor = do
    studs <- io $ Control.Vorlesung.DB.steilnehmer $ V.vnr vor
    edit_studenten studs

edit_studenten studs = do
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

resultate vor choice = do
    let vnr = V.vnr vor
    close -- btable

    sgs <- io $ Control.Vorlesung.DB.snr_gnr_teilnehmer vnr


    aufs0 <- io $ Control.Aufgabe.DB.get ( Just vnr )
    let aufs = do 
           auf <- aufs0
	   guard $ case choice of
               None -> False
               Mandat -> A.status auf == Mandatory
               All -> True
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

    h3 "Gruppen"
    open btable 
    mapM_ plain [ "Nummer", "Bezeichnung", "Referent" ]
    sequence_ $ do
        gnr <- Data.List.nub $ map snd sgs 
        return $ do
            [ g ] <- io $ Control.Gruppe.DB.get_gnr gnr
            open row
            plain $ show gnr
            plain $ toString $ G.name     g
            plain $ toString $ G.referent g
            close -- row
    close -- table

    h3 "Einsendungen (Ok/No)"

    let anames = do
        anr <- setToList anrs
	return $ case lookupFM fma anr of
	    Just name -> ( if choice == Mandat then take 3 else id )
			 $ toString name
	    Nothing   -> show anr

    let headings = [ "Matr.-Nr.", "Vorname", "Name", "Gruppe" ] ++ anames ++ [ "total" ]
    open_btable_with_sorter headings

    sequence_ $ do
        (snr, gnr) <- sgs
	return $ do
            [ stud ] <- io $ Control.Student.DB.get_snr snr
	    let mnr = S.mnr stud
                vorname = S.vorname stud
                name = S.name stud
	    open row 
	    plain $ toString mnr
            plain $ toString vorname
            plain $ toString name
            plain $ show gnr
	    nums <- sequence $ do
	        anr <- setToList $ anrs
                let result = lookupFM stud_aufg (S.snr stud, anr) 
		return $ do
		     plain $ case result of
		         Just ( Oks o, Nos n ) -> 
			     if choice == Mandat
			     then show o else show (o, n)
		         Nothing -> "-"
		     return $ case result of
			 Just ( Oks o, _ ) | o > 0 -> 1
			 _                         -> 0

            plain $ show $ sum nums
            close -- row

    close -- btable




