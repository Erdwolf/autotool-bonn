{-# LANGUAGE TemplateHaskell #-}

module RSA.Break.Data where

import Autolib.Reader
import Autolib.ToDoc
import Data.Typeable

data Config =
     Config { public_key :: ( Integer, Integer )
            , message :: Integer
	    }
     deriving ( Typeable )

example :: Config
example = Config
	{ public_key = ( 7, 55 )
	, message = 9
	}

$(derives [makeReader, makeToDoc] [''Config])

-- local variables:
-- mode: haskell
-- end:



