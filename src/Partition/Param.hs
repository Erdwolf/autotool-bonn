{-# LANGUAGE TemplateHaskell #-}

module Partition.Param where

--   $Id$

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Set ( Set )

import Data.Typeable

data Param = 
     Param { elements :: Int
	   , bounds :: ( Integer, Integer )
	   }
     deriving ( Typeable )

p :: Param
p =  Param { elements = 15
	   , bounds = ( 5, 25 )
	   }


$(derives [makeReader, makeToDoc] [''Param])

data Conf = Conf { nums ::  Set Integer } deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Conf])

-- Local Variables:
-- mode: haskell 
-- End:

