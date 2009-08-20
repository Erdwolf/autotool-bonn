-- -*- mode: haskell -*-

{-# LANGUAGE TemplateHaskell #-}
module Graph.Color where

import Data.Typeable
import Autolib.ToDoc
import Autolib.Reader
-- import Text.XML.HaXml.Haskell2Xml
import Autolib.Hash

data Color = A | B | C | D | E | F | G | H
     deriving ( Eq, Ord, Typeable, Enum, Bounded )

instance Hash Color where hash = hash . fromEnum

$(derives [makeReader, makeToDoc] [''Color])
-- {-! for Color derive: Reader, ToDoc, Haskell2Xml !-}



