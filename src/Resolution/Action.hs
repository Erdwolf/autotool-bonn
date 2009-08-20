{-# language DeriveDataTypeable #-}

module Resolution.Action where

import Resolution.Data

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Size

import Data.Typeable

data Action = Resolve { left :: Int, right :: Int, literal :: Literal }
    deriving Typeable

instance Size Action where size _ = 1

{-! for Action derive: Reader, ToDoc !-}

example :: Action
example = Resolve { left = 0, right = 1, literal = read "! x" }

-- local variables:
-- mode: haskell
-- end:
