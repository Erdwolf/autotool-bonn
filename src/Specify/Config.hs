{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-} 

module Specify.Config where

import Specify.Constraint

import Autolib.Reader
import Autolib.ToDoc

import Data.Typeable

data Config = 
     Config { constraints :: System
	    , checks_per_constraint :: Int
	    }
    deriving Typeable

$(derives [makeReader, makeToDoc] [''Config])

example :: Config
example = Config 
	{ constraints = Specify.Constraint.example
	, checks_per_constraint = 1000
	}
	
-- local variables:
-- mode: haskell
-- end:
