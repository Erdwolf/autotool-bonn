-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell #-}

module Control.Gruppe.Typ where

import Control.Types

import Autolib.Reader
import Autolib.ToDoc

import Data.Typeable

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
