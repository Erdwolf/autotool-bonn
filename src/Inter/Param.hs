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
			 -- after login key for DB
		   , snr :: String
			 -- configured
		   , variants :: [ Variant ]
	           , input_width :: Int
	                -- generated
	           , variante :: Variant
		   }

empty :: Type
empty = Param { matrikel = ""
			  , passwort = Passwort.empty
			  , problem  = ""
			  , variant  = ""
			  , input    = ""
			  , snr 	 = ""
		          , input_width = 70
			  , variants = []
			  , variante = error "empty.variante"
			  }

