{-# LANGUAGE TemplateHaskell #-}

module Graph.Cage.Config where

import Inter.Types

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

data Config = Config 
	    { girth :: Int -- ^ at least
             , chi :: Int -- ^ at least
	     }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Config])

rc :: Config
rc = Config { girth = 4
	, chi = 4
	}

-- Local Variables:
-- mode: haskell
-- End:
