{-# OPTIONS -fglasgow-exts #-}

{-# LANGUAGE TemplateHaskell #-}
module Algebraic.Config where

import qualified Autolib.TES.Binu as B
import Expression.Op

import Data.Typeable
-- import Text.XML.HaXml.Haskell2Xml

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Set

data Information = Formula | Value deriving Typeable

$(derives [makeReader, makeToDoc] [''Information])
-- {-! for Information derive: Haskell2Xml, ToDoc, Reader !-}

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
-- {-! for Type derive: Haskell2Xml, ToDoc, Reader !-}



-- local variables:
-- mode: haskell
-- end;

