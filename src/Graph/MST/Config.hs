-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell #-}

--  $Id$

module Graph.MST.Config where

import Graph.MST.Weight

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

data Config = Config 
	    { nodes       :: Int
	    , edges       :: Int
	    , weight_type :: Weight
	    }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Config])

rc :: Config
rc = Config { nodes       = 15
	    , edges       = 30
	    , weight_type = Random 12
	    }
