{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, UndecidableInstances, FlexibleContexts #-} 

module Code.Param where

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Size
import Autolib.Set

import Data.Typeable

data Encode c = Encode c deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Encode])

data Decode c = Decode c  deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Decode])

data Compress c = Compress c  deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Compress])

data ( Reader [a], ToDoc [a], Ord a ) => Config a =
     Config { alphabet :: Set a
	    , length_range :: ( Int, Int )
	    }
     deriving ( Typeable )

example :: Config Char
example = Config { alphabet = mkSet [ 'a' .. 'g' ]
		 , length_range = (10, 20)
		 }

$(derives [makeReader, makeToDoc] [''Config])

-- Local variables:
-- mode: haskell
-- End:
