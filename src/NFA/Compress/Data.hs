{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE TemplateHaskell #-}

module NFA.Compress.Data where

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

data DFA_Compress = DFA_Compress deriving Typeable 

$(derives [makeReader, makeToDoc] [''DFA_Compress])

-- local variables:
-- mode: haskell
-- end:
