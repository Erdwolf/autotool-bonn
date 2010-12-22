{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Faktor.Param where

--   $Id$

import Autolib.Reader
import Autolib.ToDoc
import Data.Typeable
import Autolib.Set

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

$(derives [makeReader, makeToDoc] [''Param])

instance Show Param where show = render . toDoc
instance Read Param where readsPrec = parsec_readsPrec

