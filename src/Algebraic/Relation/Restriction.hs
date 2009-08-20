{-# OPTIONS -fglasgow-exts #-}

module Algebraic.Relation.Restriction where

import Condition

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Size


import Autolib.Reporter

import Data.Typeable


data Restriction
    = Size_Range (Int, Int)
    deriving ( Eq, Ord, Typeable )

{-! for Restriction derive: Reader, ToDoc !-}

	    

-- local variables:
-- mode: haskell
-- end: