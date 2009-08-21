-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell #-}

module Baum.Config where

import Baum.Order

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

data Config = 
     Config { nodes  :: Int
	    , orders :: [ Order ]
	    }
     deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc] [''Config])

hash :: Config -> Int
hash conf = foldl ( \ x y -> 13 * x + y + 29 ) 0
	  $ map fromEnum
	  $ show $ orders conf

example :: Config
example =  Config
	{ nodes = 13
	, orders = [ Pre, In ]
	}

