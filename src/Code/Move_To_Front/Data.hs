{-# OPTIONS -fallow-overlapping-instances -fglasgow-exts #-}

{-# LANGUAGE TemplateHaskell #-}
module Code.Move_To_Front.Data where

import Code.Type ( BitSize (..) )

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Size

import Data.Typeable
-- import Text.XML.HaXml.Haskell2Xml

data Move_To_Front = Move_To_Front deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc] [''Move_To_Front])
-- {-! for Move_To_Front derive: Reader, ToDoc, Haskell2Xml !-}

data Coding a = Coding { queue  :: a
		       , output :: [ Int ]
		       }
     deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc] [''Coding])
-- {-! for Coding derive: Reader, ToDoc, Haskell2Xml !-}

instance Size (Coding a) where 
    size = length . output 

instance BitSize (Coding a) where
    bitSize  = fromIntegral . size 


-- Local variables:
-- mode: haskell
-- End:




     