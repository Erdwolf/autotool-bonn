module Passwort 

( Type
, empty
)

where

--   $Id$

import CGI hiding ( empty )
import Prelude hiding ( span,map, head, div )
import qualified Prelude

--
newtype Type = ATPasswort { unATPasswort :: String }

empty = ATPasswort ""


instance Read Type where
  readsPrec i str = 
	  let 
		(anf, rst ) = Prelude.span okPass str 
		okPass = not . (=='"' )
	  in 
	  if null rst 
		 then [(ATPasswort str, [] )] 
		 else [] -- throw $ PatternMatchFail str

instance Show Type where
  showsPrec i (ATPasswort str) = showString str

instance Reason Type where
  reason _ = "Passwort enthält nicht nur [a-zA-Z0-9-_ ]"

