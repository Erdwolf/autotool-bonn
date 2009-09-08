-- -*- mode: haskell -*-

{-# LANGUAGE TemplateHaskell #-}
module JVM.Type where

--   $Id$

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Data.Typeable
-- import Text.XML.HaXml.Haskell2Xml

data Statement = 
	   Push Integer	 | Drop | Dup
	 | Add | Sub | Mul -- | Div
	 | Load 	 | Store 
	 | Jump Int | Jumpz Int -- relative Sprünge
	 | Halt
    deriving ( Eq, Ord, Typeable )

type Program = [ Statement ]

flatten :: Program -> [ Statement ]
flatten p = p

instance Size Program where 
    size = sum . map size
instance Size Statement where
    size _ = 1

$(derives [makeReader, makeToDoc] [''Statement])
-- {-! for Statement derive: ToDoc, Reader, Haskell2Xml !-}



