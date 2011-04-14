{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-} 

module Graph.Bisekt.Data where

import Graph.Util
import Autolib.Set
import Autolib.ToDoc
import Autolib.Reader
import Data.Typeable

data ( GraphC a ) => Solution a = 
     Solution { schnittkanten :: Set (Kante a)
	      , knoten1 :: Set a
	      , knoten2 :: Set a
	      }
    deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc] [''Solution])

-- local variables:
-- mode: haskell
-- end:
