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

import Data.Maybe

validate :: P.Type -> IO ( Either ( Html, P.Type ) P.Type )
validate par |  isNothing ( P.mmatrikel par ) 
             || isNothing ( P.mpasswort par ) = 
      logged "validate_nothing" $ return 
         $ Left ( p << "kein matrikel oder kein passwort", par )

validate par = do

    -- FIXME: security hole
    mbsnr <- logged ( show [ "loginDB", show $ P.matrikel par, show $ P.passwort par ] ) 
	   $ loginDB (P.matrikel par) (P.passwort par)

    let msg = p << "Bitte gültige Matrikelnummer und Passwort eingeben, danach Aufgabe wählen."
    case mbsnr of 
     -- nein 
     Nothing -> return $ Left ( p << msg, par )
     -- gut nun Aufgaben-Varianten fuer User (ident) 
     -- aus ( Inter.Boiler ) holen und mit Eingaben vergleichen
     Just ident -> continue $ par { P.ident = ident } -- ist SNr

{-
        do 
        let vvs :: [ Variant ]
            vvs = do
            -- Ueber alle Varianten (mittles List-Monade)
                  vv <- P.variants par
                  case vv of 
                    Variant v -> do
                      guard $ P.saufgabe par ==       Inter.Types.tag v
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

-}

continue :: P.Type -> IO ( Either ( Html, P.Type ) P.Type )
continue set = do
   let is_admin = False 
		  -- read ( S.smatrikel set ) < 1024 -- FIXME: remove explicit number
   aufs <- logged "mglAufgabenDB'" $ mglAufgabenDB' is_admin $ S.ident set
   let names = do  
	 ( (anr, name, typ), (conf, highscore, remark) ) <- aufs
         return $ name
   let matching = do
           auf @ ( (anr, name, typ), (conf, highscore, remark) ) <- aufs
           guard $ name == S.aufgabe set 
           return auf
   logged ( "matching: " ++ show matching ) $ return ()
   logged ( "makers: " ++ show (P.makers set) ) $ return ()

   case matching of
     [ auf @ ( (anr, name, typ),( conf, highscore, remark )) ] -> do
         -- maker suchen
         case ( do mk <- P.makers set
		   guard $ show mk == toString typ 
		   return mk
	      ) of
	     [ mk @ ( Make doc ( fun :: conf -> Var p i b ) i ) ] -> 
	         -- parametersatz herstellen
	         return $ Right $ set 
		       { S.anr = anr
		       , S.variante = -- TODO: parserfehler behandeln
			    Variant $ ( fun ( read $ toString conf :: conf ) )
				      { Inter.Types.tag = toString name }
		       , S.typ = typ
		       , S.conf = conf
		       , S.highscore = highscore 
		       , S.remark = remark
		       , S.names = names
		       }
             mks -> return $ Left 
		  ( p << "nicht genau ein passender maker für diesen typ, sondern:"
		    +++ p << pre << render ( toDoc mks )
		  , set { S.names = names }
		  )

     -- fall-through: aufgabe gibt es nicht
     _ -> return $ Left
                 ( p << ( "Problem/Aufgabe/Version ist nicht aktuell oder " 
						  ++ "Sie sind nicht in der passenden Gruppe.")
                       +++ p << pre << render ( toDoc matching )
		 , set { S.names = names }
		 )






