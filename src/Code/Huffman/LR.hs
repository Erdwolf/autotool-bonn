{-# LANGUAGE TemplateHaskell #-}
module Code.Huffman.LR where

--  $Id$

import Autolib.ToDoc
import Autolib.Reader

import Code.Type

import Data.Typeable
-- import Text.XML.HaXml.Haskell2Xml

data LR = L | R 
     deriving ( Eq, Ord, Enum, Bounded, Typeable )

$(derives [makeReader, makeToDoc] [''LR])
-- {-! for LR derive : ToDoc, Reader, Haskell2Xml !-}

data Ord a => Letter a = Letter 
	      { weight :: Int
	      , codes  :: Code a LR
	      }
     
$(derives [makeReader, makeToDoc] [''Letter])
-- {-! for Letter derive : ToDoc, Reader !-}

-- local variables:
-- mode: haskell
-- end;






