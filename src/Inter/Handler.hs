module Inter.Handler where

-- -- $Id$

import Reporter

type Matrikel = String

type Key = String

data Handler p i = 
     Handler { problem :: p 
	     , variant :: String
	     , key :: Matrikel -> IO Key
	     , gen :: Matrikel -> Reporter i
	     }
