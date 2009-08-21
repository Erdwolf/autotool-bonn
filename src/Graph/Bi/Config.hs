-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell #-}

--  $Id$

module Graph.Bi.Config where

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

data Config = Config 
	    { nodes :: Int -- ^ number of nodes
	    , edges :: Int -- ^ number of edges
	    , teil  :: Int -- ^ größe des beweises im verhältnis zur knotenzahl
	    }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Config])

rc :: Config
rc = Config { nodes = 12
	    , edges = 40
	    , teil  = 3
	    }
