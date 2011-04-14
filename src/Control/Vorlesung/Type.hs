-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-} 

module Control.Vorlesung.Type where

import Control.Types

import Autolib.Reader
import Autolib.ToDoc

import Data.Typeable

-- | das sollte exactly das sein, was auch in DB-tabelle  steht

data Vorlesung =
     Vorlesung { vnr :: VNr
	       , name :: name
	       }

$(derives [makeReader, makeToDoc] [''Vorlesung])



