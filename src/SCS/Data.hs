{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances #-}

{-# LANGUAGE TemplateHaskell #-}
module SCS.Data where

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Set

import Data.Typeable
import Autolib.Xml

import Network.XmlRpc.Internals

class    ( Ord a,  ToDoc a, Reader a, Typeable a, ToDoc [a], Reader [a]
	 -- , Haskell2Xml [a] 
         , XmlRpcType [a]
	 )
    => InstanceC a 
instance ( Ord a, ToDoc a, Reader a, Typeable a,  ToDoc [a], Reader [a]
	 -- , Haskell2Xml [a] 
         , XmlRpcType [a]
	 )
    => InstanceC a 

data ( InstanceC a  ) => Instance a = 
     Instance { contents :: [[a]]
              , max_length :: Int
	      }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Instance])
-- {-! for Instance derive: Reader, ToDoc !-}

example :: Instance Char
example = Instance
	{ contents = [ "ABCABBA", "CBABAC" ]
	, max_length = 10
	}

-- Local Variables:
-- mode: haskell
-- End:
