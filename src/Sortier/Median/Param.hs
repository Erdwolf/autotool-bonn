{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Sortier.Median.Param where

import Autolib.ToDoc
import Autolib.Reader
import Data.Typeable

data Param =
     Param { breite :: Int 
	   , max_comparators :: Int 
	   }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Param])

-- local variables:
-- mode: haskell
-- end
