{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-} 

module Code.Move_To_Front.Data where

import Code.Type ( BitSize (..) )

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Size

import Data.Typeable

data Move_To_Front = Move_To_Front deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc] [''Move_To_Front])

data Coding a = Coding { queue  :: a
		       , output :: [ Int ]
		       }
     deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc] [''Coding])

instance Size (Coding a) where 
    size = length . output 

instance BitSize (Coding a) where
    bitSize  = fromIntegral . size 


-- Local variables:
-- mode: haskell
-- End:




     