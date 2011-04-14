{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-} 

module PL.Param where

import PL.Type

import Autolib.Reader
import Autolib.ToDoc

import Data.Typeable

data Param =
     Param { model_size :: Int -- TODO: Atleast, Atmost, ...
	   , formel :: Formel
	   }
     deriving ( Typeable )

example :: Param
example = Param
	{ model_size = 4
	, formel = read "forall x . exists y . R(x,y)"
	}

$(derives [makeReader, makeToDoc] [''Param])

-- local variables:
-- mode: haskell
-- end
