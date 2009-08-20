{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Diffie_Hellman.Param where

import Autolib.Reader
import Autolib.ToDoc

import Data.Typeable

data Param =
     Param { digits :: Int -- ^  recommended: between 10 and 15
           }
    deriving Typeable

example :: Param
example = Param { digits = 12 }

$(derives [makeReader, makeToDoc] [''Param])
-- {-! for Param derive: Reader, ToDoc !-}

-- local variables:
-- mode: haskell
-- end:
