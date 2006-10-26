{-# OPTIONS -fglasgow-exts #-}

module Partition.Param where

--   $Id$

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Set ( Set )

import Data.Typeable
import Text.XML.HaXml.Haskell2Xml

data Param = 
     Param { elements :: Int
	   , bounds :: ( Integer, Integer )
	   }
     deriving ( Typeable )

p :: Param
p =  Param { elements = 15
	   , bounds = ( 5, 25 )
	   }


{-! for Param derive: ToDoc, Reader, Haskell2Xml !-}

data Conf = Conf { nums ::  Set Integer } deriving ( Typeable )

{-! for Conf derive: ToDoc, Reader, Haskell2Xml !-}

-- Local Variables:
-- mode: haskell 
-- End:

