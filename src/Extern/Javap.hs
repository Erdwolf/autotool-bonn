-- TODO das ist erstmal ein ad-hoc-programm
-- sollte sp‰ter zerlegt und generalisiert werden
-- falls es sich denn lohnen sollte

--  $Id$

module Extern.Javap where

data Bytecode = Bytecode  deriving ( Eq, Ord, Show, Read )

data BC = BC { resultat :: [ String ]
	     , max_lines :: Int
	     }
    deriving ( Show )

data Java = Java String
instance Reader Java 
instance ToDoc Java 

instance C.Partial Bytecode BC Java where
    initial p i   = Java $ unlines [ "return (x0 * x1);" ]
    partial p i b = return ()
    total   p i b = undefined
        -- hier muﬂ leider ein IO-Prozess laufen!
  




