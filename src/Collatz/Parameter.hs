-- -*- mode: haskell -*-

{-# LANGUAGE TemplateHaskell #-}
module Collatz.Parameter where

import Prelude hiding ( length )
import qualified Prelude

import Autolib.Reader
import Autolib.ToDoc
-- import Text.XML.HaXml.Haskell2Xml
import Data.Typeable
import qualified Data.List

data Parameter = Parameter { length :: Integer
		   , top :: Integer
		   }
    deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc] [''Parameter])
-- {-! for Parameter derive: ToDoc, Reader, Haskell2Xml !-}

compute :: Integer -> Parameter
compute x =
        let xs = takeWhile ( > 1 )
	       $ iterate ( \ x -> if even x then div x 2 else 3 * x + 1 )
	       $ x
        in Parameter { length = Data.List.genericLength xs 
			    , top = foldr max 1 xs
			    }

one :: Parameter
one = compute 1


