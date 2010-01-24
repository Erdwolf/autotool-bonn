{-# language DeriveDataTypeable, TemplateHaskell #-}

module Baum.Such.Config where

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

data Config a = -- use phantom type for baum 
     Config { start_size   :: Int
	    , min_key :: a
	    , max_key :: a
	    , fixed_insert_ops :: Int 
	    , fixed_delete_ops :: Int 
	    , guess_insert_ops :: Int
	    , guess_delete_ops :: Int
	    }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Config])

example :: Config Int
example = Config 
	{  start_size   = 10
	, min_key = 0
	, max_key = 1000	
	, fixed_insert_ops = 5
	, fixed_delete_ops = 0 
	, guess_insert_ops = 5
	, guess_delete_ops = 0
	}
