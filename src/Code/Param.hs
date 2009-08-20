{-# OPTIONS -fallow-overlapping-instances -fglasgow-exts -fallow-undecidable-instances #-}

module Code.Param where

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Size
import Autolib.Set

import Data.Typeable
import Text.XML.HaXml.Haskell2Xml

data Encode c = Encode c deriving ( Typeable )

{-! for Encode derive:  Reader, ToDoc, Haskell2Xml !-}

data Decode c = Decode c  deriving ( Typeable )

{-! for Decode derive:  Reader, ToDoc, Haskell2Xml !-}

data Compress c = Compress c  deriving ( Typeable )

{-! for Compress derive:  Reader, ToDoc, Haskell2Xml !-}

data ( Reader [a], ToDoc [a], Ord a ) => Config a =
     Config { alphabet :: Set a
	    , length_range :: ( Int, Int )
	    }
     deriving ( Typeable )

example :: Config Char
example = Config { alphabet = mkSet [ 'a' .. 'g' ]
		 , length_range = (10, 20)
		 }

{-! for Config derive: Reader, ToDoc, Haskell2Xml !-}

-- Local variables:
-- mode: haskell
-- End:
