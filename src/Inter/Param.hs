module Inter.Param where

--   $Id$

import qualified Control.Passwort 

import Inter.Types ( Variant )
import Inter.Click
import Control.Types


--
-- CGI - Zustands-Daten
--
-- hier ist alles drin, wird von aufruf zu aufruf transportiert
--
data Type = 
    Param { -- user input
            matrikel :: MNr -- TODO: oder Maybe?
          , passwort :: Control.Passwort.Type

          , problem  :: String

          , aufgabe  :: Name
          , typ  :: Typ
	  , conf :: Config
	  , remark :: Remark

          , input    :: String
	  , wahl :: String -- vorige aufgabe
	  , click    :: Click
            -- after login key for DB
          , ident :: SNr
          , highscore :: HiLo
          , anr :: ANr
            -- configured
          , variants :: [ Variant ]
          , input_width :: Int
            -- generated
          , variante :: Variant
          }

smatrikel p = toString $ matrikel p 
saufgabe p = toString $ aufgabe p 
styp p = toString $ typ p 

subject p = toString (typ p) ++ "-" ++ toString (aufgabe p)

example :: Type
example = empty { problem  = "Computer" -- damit ein beispiel dasteht
                , aufgabe  = fromCGI "LOOP"
                , typ = fromCGI "TIMES"
                }

empty :: Type
empty = Param { matrikel = error "empty.matrikel"
              , problem  = "" 

              , aufgabe  = error "Param.empty.aufgabe"
              , typ =  error "Param.empty.typ"
              , conf =  error "Param.empty.conf"
	      , remark = error "Param.empty.remark"

              , passwort = Control.Passwort.empty
              , input    = ""
	      , wahl = ""
	      , click = Example
              , ident    = error "Param.empty.ident"
              , input_width = 80
              , variants = []
              , variante = error "Param.empty.variante"

              , highscore = error "Param.empty.highscore"
              , anr = error "Param.empty.anr"
	          }

