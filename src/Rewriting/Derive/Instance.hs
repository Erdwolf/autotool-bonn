{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Rewriting.Derive.Instance where


import Autolib.ToDoc
import Autolib.Reader


import Data.Typeable



data (  Reader system, ToDoc system
     , Reader object, ToDoc object
     ) 
    => Instance system object = Instance
        { system :: system
        , from   :: object
        , to     :: object
        }
    deriving ( Typeable 
	     )

$(derives [makeReader, makeToDoc] [''Instance])

-- local variables:
-- mode: haskell
-- end:
