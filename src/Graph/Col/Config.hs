-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-} 

module Graph.Col.Config where

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

data Config = Config 
	    { nodes :: Int -- ^ number of nodes
	     , edges :: Int -- ^ number of edges
             , chi :: Integer -- ^ chromatic number
	     }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Config])



rc :: Config
rc = Config { nodes = 10
	, edges = 20
	, chi = 4
	}
