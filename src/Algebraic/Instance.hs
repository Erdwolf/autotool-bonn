{-# OPTIONS -fglasgow-exts #-}

{-# LANGUAGE TemplateHaskell #-}
module Algebraic.Instance where

import Expression.Op
import qualified Autolib.TES.Binu as B

import Data.Typeable
-- import Text.XML.HaXml.Haskell2Xml

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Set

data Ops a => Type a =
     Make { target :: a
          , description :: Maybe String
	  , operators :: B.Binu ( Op a )
          , max_size :: Int
	  }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Type])
-- {-! for Type derive: Haskell2Xml, ToDoc, Reader !-}

-- local variables:
-- mode: haskell
-- end;
