{-# OPTIONS -fglasgow-exts #-}

module Fun.Quiz.Type2 where

import Fun.Type

import Autolib.Reader
import Autolib.ToDoc

import Data.Typeable

data Param = Param
	   { expression_size :: Int
	   , table_size :: Int
	   , properties :: [ Property ]
	   }
    deriving ( Typeable )

example :: Param
example = Param { expression_size = 10
		, table_size = 10
		, properties = [ Builtins [] ]
		}

{-! for Param derive : Reader, ToDoc !-}

-- local variables:
-- mode: haskell
-- end:
