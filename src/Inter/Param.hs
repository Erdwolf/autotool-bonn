module Inter.Param where

-- $Id$

import qualified Passwort -- control

import Inter.Types ( Variant )
import SQLqueries ( ATHighLow )

data Type = 
     Param { -- user input
             matrikel :: String
	   , passwort :: Passwort.Type
	   , problem  :: String
           , aufgabe  :: String -- major
	   , version  :: String -- minor
     	   , input    :: String
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

empty :: Type
empty = Param { matrikel = ""
			  , passwort = Passwort.empty
			  , problem  = ""
			  , aufgabe  = ""
	                  , version = ""
			  , input    = ""
			  , ident 	 = ""
		          , input_width = 70
			  , variants = []
			  , variante = error "empty.variante"
			  }

