-- -*- mode: haskell -*-

--  $Id$

module Graph.Way.Config where

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable
import Text.XML.HaXml.Haskell2Xml

data Config = Config 
	    { nodes :: Int
	    , edges :: Int
	    }
     deriving ( Typeable )

{-! for Config derive: Reader, ToDoc, Haskell2Xml !-}

rc :: Config
rc = Config { nodes        = 12
	    , edges        = 9
	    }
