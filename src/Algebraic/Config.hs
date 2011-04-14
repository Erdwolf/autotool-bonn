{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-} 

module Algebraic.Config where

import qualified Autolib.TES.Binu as B
import Expression.Op

import Data.Typeable

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Set

data Information = Formula | Value deriving Typeable

$(derives [makeReader, makeToDoc] [''Information])

data Ops a => Type c a =
     Make { max_formula_size_for_instance :: Int
	 , operators_in_instance :: B.Binu ( Op a )
	 , operators_in_solution :: B.Binu ( Op a )
	 , restrictions :: [ c ]
         , information :: Information
	 , max_formula_size_for_solution :: Int
	 }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Type])



-- local variables:
-- mode: haskell
-- end;

