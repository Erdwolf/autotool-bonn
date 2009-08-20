-- -*- mode: haskell -*-

module Graph.Col.Config where

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable
import Text.XML.HaXml.Haskell2Xml

data Config = Config 
	    { nodes :: Int -- ^ number of nodes
	     , edges :: Int -- ^ number of edges
             , chi :: Integer -- ^ chromatic number
	     }
     deriving ( Typeable )

{-! for Config derive: Reader, ToDoc, Haskell2Xml !-}



rc :: Config
rc = Config { nodes = 10
	, edges = 20
	, chi = 4
	}
