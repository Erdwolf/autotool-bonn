-- -*- mode: haskell -*-

{-# LANGUAGE TemplateHaskell #-}
module Baum.Order where

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable
-- import Text.XML.HaXml.Haskell2Xml

data Order = Pre | In | Post | Level
     deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc] [''Order])
-- {-! for Order derive: ToDoc, Reader, Haskell2Xml !-}

