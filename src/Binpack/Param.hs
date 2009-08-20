{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Binpack.Param where

import Autolib.Reader
import Autolib.ToDoc
import Data.Typeable

data Param = 
     Param { capacity :: Integer
           , bins :: Int
           }
    deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Param])
-- {-! for Param derive: Reader, ToDoc !-}

example :: Param
example = Param { capacity = 20, bins = 10 }

-- local variables:
-- mode: haskell
-- end:
