-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell #-}

module Faktor.Euklid.Param where

--   $Id$

import Autolib.Reader
import Autolib.ToDoc
import Data.Typeable

data Param = 
     Param { von :: Integer
	   , bis :: Integer
           , max_ggt :: Integer
	   }
     deriving ( Typeable )

p :: Param
p = Param { von = 1000
	  , bis = 5000
          , max_ggt = 10
	  }

$(derives [makeReader, makeToDoc] [''Param])


