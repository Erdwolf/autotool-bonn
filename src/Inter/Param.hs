module Inter.Param where

-- $Id$

import qualified Passwort -- control

import Inter.Types ( Variant )


data Type = 
     Param { -- user input
	     matrikel :: String
	   , passwort :: Passwort.Type
	   , problem  :: String
	   , variant  :: String
	   , input    :: String

	   -- configured
	   , variants :: [ Variant ]
	   }

empty :: Type
empty = Param { matrikel = ""
	   , passwort = Passwort.empty
	   , problem  = ""
	   , variant  = ""
	   , input    = ""

	   , variants = []
	   }
