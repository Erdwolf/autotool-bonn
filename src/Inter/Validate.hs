module Inter.Validate where

-- $Id$

import Inter.Types
import qualified Inter.Param as P

-- das wird sp�ter mal autotool/bin/Settings.hs
import qualified Inter.Param as S 

import Text.Html
import SQLqueries

import Monad ( guard )
import ToDoc

validate :: P.Type -> IO ( Either Html P.Type )
validate par = do
    -- passwort vergleich mit db
    let matrikel = P.matrikel par
    let passwort = show $ P.passwort par
    mbsnr <- loginDB matrikel passwort    

    let msgstr = 
            if length matrikel > 0 || length passwort > 0
            then "Passwort pa�t nicht"
            else "Bitte Matrikelnr., Passwort eingeben und Problem/Aufgabe/Version w�hlen."

    case mbsnr of 
     -- nein 
     Nothing -> return $ Left $ p << msgstr 
     -- gut nun Aufgaben-Varianten fuer User (ident) 
     -- aus ( Inter.Boiler ) holen und mit Eingaben Vergleichen
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
                      guard $ P.aufgabe par ==        Inter.Types.aufgabe v
                      guard $ P.version par ==        Inter.Types.version v
                      return vv
        case vvs of
                 [ vv ] -> continue $ par { P.ident = ident
                                          , P.variante = vv
                                          , P.variants = P.variants par
                                          }
                 _      -> return 
                           $ Left 
                           $ p << "m�gliche Probleme/Aufgaben/Versionen:"
                                 +++ pre << render ( toDoc $ P.variants par )

continue :: P.Type -> IO ( Either Html P.Type )
continue set = do
   let is_admin = read ( S.matrikel set ) < 1024
   aufs <- mglAufgabenDB' is_admin $ S.ident set
   let matching = 
           do
           auf @ ( anr, name, subject, path, highscore ) <- aufs
           guard $ name == S.aufgabe set && subject == S.version set
           return auf
   case matching of
     [ auf @ ( anr, name, subject, path, highscore ) ] -> 
                 return 
                 $ Right 
                 $ set { S.anr = anr , S.highscore = read highscore }
     _ -> return $ Left
                 $ p << ( "Problem/Aufgabe/Version ist nicht aktuell oder " 
						  ++ "Sie sind nicht in der passenden Gruppe.")
                       +++ p << pre << render ( toDoc matching )






