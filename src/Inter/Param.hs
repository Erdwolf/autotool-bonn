module Inter.Param where

-- $Id$

import Inter.Types ( Variant )

data Type = 
     Param { -- user input
	     matrikel :: String
	   , passwort :: String
	   , problem  :: String
	   , variant  :: String
	   , input    :: String
	   -- configured
	   , variants :: [ Variant ]
	   }

empty :: Type
empty = Param { matrikel = ""
	   , passwort = ""
	   , problem  = ""
	   , variant  = ""
	   , input    = ""
	   , variants = []
	   }
