{-# LANGUAGE TemplateHaskell #-}
module Graph.Bisekt.Data where

import Graph.Util
import Autolib.Set
import Autolib.ToDoc
import Autolib.Reader
-- import Text.XML.HaXml.Haskell2Xml
import Data.Typeable

data ( GraphC a ) => Solution a = 
     Solution { schnittkanten :: Set (Kante a)
	      , knoten1 :: Set a
	      , knoten2 :: Set a
	      }
    deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc] [''Solution])
-- {-! for Solution derive: Reader, ToDoc, Haskell2Xml !-}

-- local variables:
-- mode: haskell
-- end:
