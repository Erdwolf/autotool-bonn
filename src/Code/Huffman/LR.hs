{-# LANGUAGE TemplateHaskell #-}

module Code.Huffman.LR where

--  $Id$

import Autolib.ToDoc
import Autolib.Reader

import Code.Type

import Data.Typeable

data LR = L | R 
     deriving ( Eq, Ord, Enum, Bounded, Typeable )

$(derives [makeReader, makeToDoc] [''LR])

data Ord a => Letter a = Letter 
	      { weight :: Int
	      , codes  :: Code a LR
	      }
     
$(derives [makeReader, makeToDoc] [''Letter])

-- local variables:
-- mode: haskell
-- end;






