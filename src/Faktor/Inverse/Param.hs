{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Faktor.Inverse.Param where

--   $Id$

import Autolib.Reader
import Autolib.ToDoc
import Data.Typeable

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

$(derives [makeReader, makeToDoc] [''Param])


