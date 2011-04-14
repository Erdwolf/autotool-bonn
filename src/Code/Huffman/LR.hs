{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-} 

module Code.Huffman.LR where

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Symbol
import Autolib.Hash
import Autolib.Size

import Code.Type

import Data.Typeable

data LR = L | R 
     deriving ( Eq, Ord, Enum, Bounded, Typeable )

$(derives [makeReader, makeToDoc] [''LR])

instance Symbol LR 
instance Size LR where size _ = 1
instance Hash LR where hash = hash . fromEnum

data Ord a => Letter a = Letter 
	      { weight :: Int
	      , codes  :: Code a LR
	      }
     
$(derives [makeReader, makeToDoc] [''Letter])







