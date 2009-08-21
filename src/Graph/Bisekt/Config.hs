-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell #-}

--  $Id$

module Graph.Bisekt.Config where

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

data Config = Config 
	    { nodes        :: Int -- ^ knoten insgesamt
	    , edges        :: Int -- ^ kanten ohne bisektierende kanten
	    , edges_bisect :: Int -- ^ bisektierende kanten
	    }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Config])

rc :: Config
rc = Config { nodes        = 10
	    , edges        = 30
	    , edges_bisect = 5
	    }
