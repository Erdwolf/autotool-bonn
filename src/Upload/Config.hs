{-# LANGUAGE TemplateHaskell #-}

module Upload.Config where

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

data Config =
     Config { aufgabe :: String
	    , punkte  :: Integer
	    }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Config])

example :: Config 
example = Config
	{ aufgabe = "(Aufgabenstellung)"
        , punkte  = 1
	}

-- Local Variables:
-- mode: haskell
-- End:
