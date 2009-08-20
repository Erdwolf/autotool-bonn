{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

{-# LANGUAGE TemplateHaskell #-}
module Upload.Config where

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable
-- import Text.XML.HaXml.Haskell2Xml

data Config =
     Config { aufgabe :: String
	    , punkte  :: Integer
	    }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Config])
-- {-! for Config derive: Reader, ToDoc, Haskell2Xml !-}

example :: Config 
example = Config
	{ aufgabe = "(Aufgabenstellung)"
        , punkte  = 1
	}

-- Local Variables:
-- mode: haskell
-- End:
