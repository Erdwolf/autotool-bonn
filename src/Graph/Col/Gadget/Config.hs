{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Graph.Col.Gadget.Config where

import Graph.Util

import Autolib.Set
import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

data GraphC a => Config a =
     Config { connectors :: Set a
	    , colors :: Int
	    , max_size :: Int
	    }
    deriving Typeable

$(derives [makeReader, makeToDoc] [''Config])

example :: Config Int
example = Config
	{ connectors = mkSet [ 1 .. 4 ]
	, colors = 3
	, max_size = 11
	}

-- local variables:
-- mode: haskell
-- end:
