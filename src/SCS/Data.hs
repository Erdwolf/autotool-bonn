{-# LANGUAGE TemplateHaskell, FlexibleContexts, FlexibleInstances #-}
{-# language UndecidableInstances, DeriveDataTypeable #-}

module SCS.Data where

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
     Instance { contents :: [[a]]
              , max_length :: Int
	      }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Instance])

example :: Instance Char
example = Instance
	{ contents = [ "ABCABBA", "CBABAC" ]
	, max_length = 10
	}

-- Local Variables:
-- mode: haskell
-- End:
