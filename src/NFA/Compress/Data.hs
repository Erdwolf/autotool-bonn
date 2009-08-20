{-# OPTIONS -fglasgow-exts #-}

module NFA.Compress.Data where

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

data DFA_Compress = DFA_Compress deriving Typeable 

{-! for DFA_Compress derive: Reader, ToDoc !-}

-- local variables:
-- mode: haskell
-- end:
