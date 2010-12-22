{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Faktor.Times.Param where

--   $Id$

import Autolib.Reader
import Autolib.ToDoc
import Data.Typeable
import Autolib.Set

data Param = 
     Param { anzahl :: Int
           , stellen :: Int
	   }
     deriving ( Typeable )

example :: Param
example = Param { anzahl = 4
	  , stellen = 20
	  }

$(derives [makeReader, makeToDoc] [''Param])


