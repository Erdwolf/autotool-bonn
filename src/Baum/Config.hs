-- -*- mode: haskell -*-

module Baum.Config where

import Baum.Order

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable
import Text.XML.HaXml.Haskell2Xml

data Config = 
     Config { nodes  :: Int
	    , orders :: [ Order ]
	    }
     deriving ( Eq, Ord, Typeable )

{-! for Config derive: ToDoc, Reader, Haskell2Xml !-}

hash :: Config -> Int
hash conf = foldl ( \ x y -> 13 * x + y + 29 ) 0
	  $ map fromEnum
	  $ show $ orders conf

example :: Config
example =  Config
	{ nodes = 13
	, orders = [ Pre, In ]
	}

