module Inter.Param where

--   $Id$

import qualified Control.Passwort 

import Inter.Types ( Variant, Make )
import Inter.Click
import Control.Types


-- | CGI - Zustands-Daten
data Type = 
    Param { -- | configured
            makers :: [ Make ]
          , input_width :: Int
	  -- | user input
          , mmatrikel :: Maybe MNr
          , mpasswort :: Maybe Control.Passwort.Type

          , aufgabe  :: Name
          , typ  :: Typ
	  , conf :: Config
	  , remark :: Remark

          , input    :: String
	  , wahl :: String -- ^ vorige aufgabe
	  , click    :: Click
            -- | after login key for DB
          , ident :: SNr
          , highscore :: HiLo
          , anr :: ANr
	  , names :: [ Name ]
            -- | generated
          , variante :: Variant
          }
    deriving Show

matrikel p = case mmatrikel p of
    Just m -> m
    Nothing -> error "Inter.Param.mmatrikel = Nothing"

passwort p = case mpasswort p of
    Just m -> m
    Nothing -> error "Inter.Param.mpasswort = Nothing"

smatrikel p = toString $ matrikel p 
saufgabe p = toString $ aufgabe p 
styp p = toString $ typ p 

subject p = toString (typ p) ++ "-" ++ toString (aufgabe p)

{-
example :: Type
example = empty { problem  = "Computer" -- damit ein beispiel dasteht
                , aufgabe  = fromCGI "LOOP"
                , typ = fromCGI "TIMES"
                }
-}

empty :: Type
empty  = Param { makers = []
	      , mmatrikel = Nothing
              , mpasswort = Nothing

              , aufgabe  = error "Param.empty.aufgabe"
              , typ =  error "Param.empty.typ"
              , conf =  error "Param.empty.conf"
	      , remark = error "Param.empty.remark"

              , input  = ""
	      , wahl = ""
	      , click = Example
              , ident    = error "Param.empty.ident"
              , input_width = 80

              , variante = error "Param.empty.variante"
	      , names = []

              , highscore = error "Param.empty.highscore"
              , anr = error "Param.empty.anr"
	          }

