-- -*- mode: haskell -*-

--  $Id$

{-# LANGUAGE TemplateHaskell #-}
module Graph.Circle.Config where

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable
-- import Text.XML.HaXml.Haskell2Xml

data Config = Config 
	    { nodes_circle   :: Int -- ^ knoten im kreis
	    , nodes_complete :: Int -- ^ knoten insgesamt
	    , edges          :: Int -- ^ extra kanten 
	    }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Config])
-- {-! for Config derive: Reader, ToDoc, Haskell2Xml !-}

rc :: Config
rc = Config { nodes_circle   = 5
	    , nodes_complete = 10
	    , edges          = 20
	    }
