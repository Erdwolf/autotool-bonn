-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell #-}

module Control.Vorlesung.Typ where

import Control.Types

import Autolib.Reader
import Autolib.ToDoc

import Data.Typeable

-- | das sollte exactly das sein, was auch in DB-tabelle  steht

data Vorlesung =
     Vorlesung { vnr :: VNr
	       , unr :: UNr
	       , enr :: ENr
	       , name :: Name
	       , einschreibVon :: Time
	       , einschreibBis :: Time
	       -- | nicht in DB, aber bei Zugriff ausgerechnet
	       , einschreib :: TimeStatus
               , motd :: Name -- ^ message of the day
	       }
     deriving Typeable

$(derives [makeReader, makeToDoc] [''Vorlesung])



