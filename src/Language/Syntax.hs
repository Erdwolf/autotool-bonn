{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-} 

module Language.Syntax where

import Grammatik.Type

import Autolib.Reader
import Autolib.ToDoc

import Data.Typeable

data Type
     = ABCdiff
     | Gleich String [Int]
     | Ordered_Gleich String
     | Ordered_Ungleich String
     | AK String
     | AmBnCmDn
     | BinCompare
     | Cee Char Type
     | Lukas
     | NoLukas
     | Dyck
     | NoDyck
     | Pali String
     | NoPali String
     | Potenzen Int
     | Power String Int
     | NoPower String Int
     | Vielfache Int
     | Form String
     | Reg String String
     | Regular String String
     | From_Grammatik Grammatik
     | Center String Char
     | Uneps Type
     | Komplement Type
     | Mirror Type
   deriving ( Eq, Typeable )

$(derives [makeReader, makeToDoc] [''Type])

-- local variables:
-- mode: haskell
-- end:

