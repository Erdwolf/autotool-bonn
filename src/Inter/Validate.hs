module Inter.Validate where

--   $Id$

import Inter.Types
import Inter.Logged
import qualified Inter.Param as P

-- das wird später mal autotool/bin/Settings.hs
import qualified Inter.Param as S 

import Text.Html
-- import SQLqueries hiding ( logged )

import Control.Types 
import Control.Queries
import Control.Punkt ( loginDB )

import Control.Monad ( guard )
import Autolib.ToDoc

validate :: P.Type -> IO ( Either Html P.Type )
validate par = do
    -- passwort vergleich mit db
    let passwort = show $ P.passwort par
    mbsnr <- logged ( show [ "loginDB", P.smatrikel par, "<passwort>" ] ) 
	   $ loginDB (P.matrikel par) passwort    

    let msgstr = 
            if {- length P.smatrikel > 0 || -} length passwort > 0
            then "Passwort paßt nicht"
            else "Bitte Matrikelnummer und Passwort eingeben sowie Problem/Aufgabe/Version wählen."

    case mbsnr of 
     -- nein 
     Nothing -> return $ Left $ p << msgstr 
     -- gut nun Aufgaben-Varianten fuer User (ident) 
     -- aus ( Inter.Boiler ) holen und mit Eingaben vergleichen
     Just ident -> 
        do 
        let vvs :: [ Variant ]
            vvs = do
            -- Ueber alle Varianten (mittles List-Monade)
                  vv <- P.variants par
                  case vv of 
                    Variant v ->
                      do
                      -- vergleiche nacheinander Problem-Typ,Aufgabe,Version
                      -- wenn OK -> return vv
                      -- Abbruch bei Fehler (=wenn guard fehlschlaegt) return []
                      guard $ P.problem par == show ( Inter.Types.problem v )
                      guard $ P.styp par ==         Inter.Types.aufgabe v
                      guard $ P.saufgabe par ==       Inter.Types.version v
                      return vv
        case vvs of
                 [ vv ] -> continue $ par { P.ident = ident
                                          , P.variante = vv
                                          , P.variants = P.variants par
                                          }
                 _      -> return 
                           $ Left 
                           $ p << "mögliche Probleme/Aufgaben/Versionen:"
                                 +++ pre << render ( toDoc $ P.variants par )

continue :: P.Type -> IO ( Either Html P.Type )
continue set = do 
   let is_admin = read ( S.smatrikel set ) < 1024 -- FIXME: remove explicit number
   aufs <- mglAufgabenDB' is_admin $ S.ident set
   let matching = 
           do
           auf @ ( (anr, name, typ), (conf, highscore, remark) ) <- aufs
           guard $ name == S.aufgabe set && typ == S.typ set
           return auf
   case matching of
     [ auf @ ( (anr, name, typ),( conf, highscore, remark )) ] -> 
                 return 
                 $ Right 
                 $ set { S.anr = anr
		       , S.conf = conf
		       , S.highscore = highscore 
		       , S.remark = remark
		       }

     -- new sub-case: aufgabe ist noch nicht in db,
     -- aber nutzer ist admin: also darf er das
     -- wir müssen dann aber die fehlenden DB-einträge raten
     _ | is_admin ->
                 return 
                 $ Right 
                 $ set { S.anr = read "0" , S.highscore = Keine }

     -- fall-through: aufgabe gibt es nicht
     _ -> return $ Left
                 $ p << ( "Problem/Aufgabe/Version ist nicht aktuell oder " 
						  ++ "Sie sind nicht in der passenden Gruppe.")
                       +++ p << pre << render ( toDoc matching )






