module Inter.Param where

--   $Id$

import qualified Passwort -- control

import Inter.Types ( Variant )
import Inter.Click
import SQLqueries ( ATHighLow )


--
-- CGI - Zustands-Daten
--
-- hier ist alles drin, wird von aufruf zu aufruf transportiert
--
data Type = 
    Param { -- user input
            matrikel :: String
          , passwort :: Passwort.Type
          , problem  :: String
          , aufgabe  :: String -- major
          , version  :: String -- minor
          , input    :: String
	  , wahl :: String -- vorige aufgabe
	  , click    :: Click
            -- after login key for DB
          , ident :: String
          , highscore :: ATHighLow
          , anr :: String
            -- configured
          , variants :: [ Variant ]
          , input_width :: Int
            -- generated
          , variante :: Variant
          }

subject p = aufgabe p ++ "-" ++ version p

example :: Type
example = empty { problem  = "Computer" -- damit ein beispiel dasteht
                , aufgabe  = "LOOP"
                , version = "TIMES"
                }

empty :: Type
empty = Param { matrikel = "" 
              , problem  = "" 
              , aufgabe  = ""
              , version = ""
              , passwort = Passwort.empty
              , input    = ""
	      , wahl = ""
	      , click = Example
              , ident    = ""
              , input_width = 80
              , variants = []
              , variante = error "empty.variante"

              , highscore = error "empty.highscore"
              , anr = error "empty.anr"
	          }

