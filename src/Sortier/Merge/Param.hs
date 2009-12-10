{-# LANGUAGE TemplateHaskell #-}

module Sortier.Merge.Param where

import Autolib.ToDoc
import Autolib.Reader
import Data.Typeable

data Param =
     Param { breiten :: [ Int ] -- ^ der bereits geordneten Teilfolgen
	   , max_comparators :: Int 
	   }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Param])

-- local variables:
-- mode: haskell
-- end
