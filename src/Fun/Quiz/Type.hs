{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-} 

module Fun.Quiz.Type where

import Autolib.Reader
import Autolib.ToDoc

import Data.Typeable

data Fun_Quiz2 = Fun_Quiz2 deriving ( Eq, Ord, Read, Show, Typeable )

$(derives [makeReader, makeToDoc] [''Fun_Quiz2])

data Param = Param
	   { expression_size :: Int
	   , table_size :: Int
	   }
    deriving ( Typeable )

example :: Param
example = Param { expression_size = 10
		, table_size = 10
		}

$(derives [makeReader, makeToDoc] [''Param])

-- local variables:
-- mode: haskell
-- end:
