{-# language DeriveDataTypeable #-}

module Binpack.Instance where

import Autolib.Reader
import Autolib.ToDoc
import Data.Typeable

data Binpack = Binpack
    deriving ( Eq, Ord, Typeable )

{-! for Binpack derive: Reader, ToDoc !-}

data Instance =
     Instance { weights :: [ Integer ]
              , capacity :: Integer
              , bins :: Int
              }
    deriving Typeable

{-! for Instance derive: Reader, ToDoc !-}

type Assignment = [[Integer]]

-- local variables:
-- mode: haskell
-- end:
