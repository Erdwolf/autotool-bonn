-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-} 

--  $Id$

module Graph.Way.Config where

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

data Config = Config 
	    { nodes :: Int
	    , edges :: Int
	    }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Config])

rc :: Config
rc = Config { nodes        = 12
	    , edges        = 9
	    }
