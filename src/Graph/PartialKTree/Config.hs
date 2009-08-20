-- -*- mode: haskell -*-

module Graph.PartialKTree.Config where

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable
import Text.XML.HaXml.Haskell2Xml

data Config = 
     Config { nodes :: Int
	    , width :: Int -- of clique
	    }
     deriving ( Typeable )

{-! for Config derive: Reader, ToDoc, Haskell2Xml !-}

example :: Config
example = Config 
	{ nodes = 10
	, width = 2
	}

