{-# OPTIONS -fglasgow-exts #-}

{-# LANGUAGE TemplateHaskell #-}
module Algebraic2.Instance where

import Expression.Op
import qualified Autolib.TES.Binu as B
import Autolib.TES.Identifier

import Data.Typeable
-- import Text.XML.HaXml.Haskell2Xml

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Set
import Autolib.FiniteMap

data Ops a => Type c a =
     Make { context :: c
	  , target :: a
          , description :: Maybe String
	  , operators :: B.Binu ( Op a )
	  , predefined :: FiniteMap Identifier a
          , max_size :: Int
	  }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Type])
-- {-! for Type derive: ToDoc, Reader !-}

-- local variables:
-- mode: haskell
-- end;
