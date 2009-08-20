-- -*- mode: haskell -*-

{-# LANGUAGE TemplateHaskell #-}
module Control.Vorlesung.Type where

import Control.Types

import Autolib.Reader
import Autolib.ToDoc

import Data.Typeable
-- import Text.XML.HaXml.Haskell2Xml hiding ( Name )

-- | das sollte exactly das sein, was auch in DB-tabelle  steht

data Vorlesung =
     Vorlesung { vnr :: VNr
	       , name :: name
	       }

$(derives [makeReader, makeToDoc] [''Vorlesung])
-- {-! for Vorlesung derive: Reader, ToDoc, Haskell2Xml !-}



