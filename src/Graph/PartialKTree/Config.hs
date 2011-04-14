-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-} 

module Graph.PartialKTree.Config where

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

data Config = 
     Config { nodes :: Int
	    , width :: Int -- of clique
	    }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Config])

example :: Config
example = Config 
	{ nodes = 10
	, width = 2
	}

