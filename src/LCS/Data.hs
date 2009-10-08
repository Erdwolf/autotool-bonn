{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances #-}
{-# LANGUAGE TemplateHaskell #-}

module LCS.Data where

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Set

import Data.Typeable
import Autolib.Xml

class    ( Ord a,  ToDoc a, Reader a, Typeable a, ToDoc [a], Reader [a]
	 )
    => InstanceC a 
instance ( Ord a, ToDoc a, Reader a, Typeable a,  ToDoc [a], Reader [a]
	 )
    => InstanceC a 

data ( InstanceC a  ) => Instance a = 
     Instance { left :: [a]
	      , right :: [a]
	      , sharp :: Bool
	      }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Instance])

example :: Instance Char
example = Instance
	{ left = "ABCABBA"
	, right = "CBABAC"
	, sharp = True
	}

-- Local Variables:
-- mode: haskell
-- End:
