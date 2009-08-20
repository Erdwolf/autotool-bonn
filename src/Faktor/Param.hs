-- -*- mode: haskell -*-

module Faktor.Param where

--   $Id$

import Autolib.Reader
import Autolib.ToDoc
import Data.Typeable
import Autolib.Set
import Text.XML.HaXml.Haskell2Xml

data Param = 
     Param { von :: Int
	   , bis :: Int
	   , anzahl :: Int
	   }
     deriving ( Typeable )

p :: Param
p = Param { von = 100
	  , bis = 1000
	  , anzahl = 3
	  }

{-! for Param derive: ToDoc, Reader, Haskell2Xml !-}

instance Show Param where show = render . toDoc
instance Read Param where readsPrec = parsec_readsPrec

