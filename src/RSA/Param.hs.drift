module RSA.Param where

import Autolib.Reader
import Autolib.ToDoc

import Data.Typeable
import Text.XML.HaXml.Haskell2Xml
import Autolib.Set

data Param = 
     Param { von :: Int
	   , bis :: Int
	   }
     deriving ( Typeable )

example :: Param
example = Param { von = 50
	  , bis = 70
	  }

{-! for Param derive: ToDoc, Reader, Haskell2Xml !-}

-- local variables:
-- mode: haskell
-- end:


