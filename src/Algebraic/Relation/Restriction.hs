{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE TemplateHaskell #-}

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

$(derives [makeReader, makeToDoc] [''Restriction])

	    

-- local variables:
-- mode: haskell
-- end: