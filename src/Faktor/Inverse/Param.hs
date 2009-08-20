-- -*- mode: haskell -*-

module Faktor.Inverse.Param where

--   $Id$

import Autolib.Reader
import Autolib.ToDoc
import Data.Typeable
import Text.XML.HaXml.Haskell2Xml

data Param = 
     Param { von :: Integer
	   , bis :: Integer
	   , hat_inverses_in_faellen_von_hundert :: Integer
	   }
     deriving ( Typeable )

p :: Param
p = Param { von = 1000
	  , bis = 5000
	  , hat_inverses_in_faellen_von_hundert = 90
	  }

{-! for Param derive: ToDoc, Reader, Haskell2Xml !-}


