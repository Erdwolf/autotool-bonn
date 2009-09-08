{-# OPTIONS -fglasgow-exts #-}

module Fun.Quiz.Type where

import Autolib.Reader
import Autolib.ToDoc

import Text.XML.HaXml.Haskell2Xml
import Data.Typeable

data Fun_Quiz2 = Fun_Quiz2 deriving ( Eq, Ord, Read, Show, Typeable )

{-! for Fun_Quiz2 derive : Reader, ToDoc !-}

data Param = Param
	   { expression_size :: Int
	   , table_size :: Int
	   }
    deriving ( Typeable )

example :: Param
example = Param { expression_size = 10
		, table_size = 10
		}

{-! for Param derive : Reader, ToDoc, Haskell2Xml !-}

-- local variables:
-- mode: haskell
-- end:
