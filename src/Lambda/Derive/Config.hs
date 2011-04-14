{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-} 

module Lambda.Derive.Config where

import Lambda.Type hiding ( free_variables )

import Autolib.Set
import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

data Type = Make
          { start_size_range :: (Int, Int)
          , free_variables :: Set Identifier
	  , overall_size_range :: (Int, Int)
          , derivation_length :: Int
          , require_exact_length :: Bool
          }
     deriving ( Typeable, Eq, Ord )

$(derives [makeReader, makeToDoc] [''Type])

example :: Type
example = Make
          { start_size_range = (15, 25)
          , free_variables = mkSet $ read "[ x , y ]"
	  , overall_size_range = (5, 50)
          , derivation_length = 5
          , require_exact_length = True
          }

-- local variables:
-- mode: haskell
-- end