-- -*- mode: haskell -*-

--  $Id$

module Graph.MST.Config where

import Graph.MST.Weight

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable
import Text.XML.HaXml.Haskell2Xml

data Config = Config 
	    { nodes       :: Int
	    , edges       :: Int
	    , weight_type :: Weight
	    }
     deriving ( Typeable )

{-! for Config derive: Reader, ToDoc, Haskell2Xml !-}

rc :: Config
rc = Config { nodes       = 15
	    , edges       = 30
	    , weight_type = Random 12
	    }
