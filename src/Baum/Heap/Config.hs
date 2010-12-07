{-# language DeriveDataTypeable, TemplateHaskell #-}

module Baum.Heap.Config where

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

data Config a = 
     Config { start_size   :: Int
	    , min_key :: a
	    , max_key :: a
	    , fixed_insert_ops :: Int 
	    , fixed_deleteMin_ops :: Int 
	    , fixed_decreaseTo_ops :: Int 
	    , guessed_insert_ops :: Int
	    , guessed_deleteMin_ops :: Int
	    , guessed_decreaseTo_ops :: Int 
	    }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Config])

example :: Config Int
example = Config 
	{  start_size   = 10
	, min_key = 0
	, max_key = 1000	
	, fixed_insert_ops = 5
	, fixed_deleteMin_ops = 2
	, fixed_decreaseTo_ops = 2
	, guessed_insert_ops = 5
	, guessed_deleteMin_ops = 2 
	, guessed_decreaseTo_ops = 2
	}

