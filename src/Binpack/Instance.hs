{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Binpack.Instance where

import Autolib.Reader
import Autolib.ToDoc
import Data.Typeable

data Binpack = Binpack
    deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc] [''Binpack])

data Instance =
     Instance { weights :: [ Integer ]
              , capacity :: Integer
              , bins :: Int
              }
    deriving Typeable

$(derives [makeReader, makeToDoc] [''Instance])

type Assignment = [[Integer]]

-- local variables:
-- mode: haskell
-- end:
