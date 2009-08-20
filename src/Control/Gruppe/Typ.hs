-- -*- mode: haskell -*-

{-# LANGUAGE TemplateHaskell #-}
module Control.Gruppe.Typ where

import Control.Types

import Autolib.Reader
import Autolib.ToDoc

import Data.Typeable
-- import Text.XML.HaXml.Haskell2Xml hiding ( Name )

-- | das sollte exactly das sein, was auch in DB-tabelle  steht

data Gruppe =
     Gruppe { gnr :: GNr
	       , vnr :: VNr
	       , name :: Name
	       , maxStudents :: Integer
	       , referent :: Name
	       }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Gruppe])
-- {-! for Gruppe derive: Reader, ToDoc, Haskell2Xml !-}



