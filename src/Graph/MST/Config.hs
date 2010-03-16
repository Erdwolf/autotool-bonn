{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Graph.MST.Config where

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

data Config = Config 
	    { nodes       :: Int
	    , edges       :: Int
	    , weight_bounds :: (Int,Int)
	    }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Config])

rc :: Config
rc = Config { nodes       = 15
	    , edges       = 30
	    , weight_bounds = ( 1, 100 )
	    }
