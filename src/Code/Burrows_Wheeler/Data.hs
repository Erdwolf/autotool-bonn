{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-} 

module Code.Burrows_Wheeler.Data where

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Size

import Data.Typeable

data Burrows_Wheeler = Burrows_Wheeler deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc] [''Burrows_Wheeler])


-- Local variables:
-- mode: haskell
-- End:
