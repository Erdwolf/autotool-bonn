{-# LANGUAGE TemplateHaskell #-}

module RSA.Param where

import Autolib.Reader
import Autolib.ToDoc

import Data.Typeable
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

$(derives [makeReader, makeToDoc] [''Param])

-- local variables:
-- mode: haskell
-- end:


