{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-} 

module Control.Semester.Typ where

import Control.Types 

import Autolib.Reader
import Autolib.ToDoc

import Data.Typeable

-- | das sollte exactly das sein, was auch in DB-tabelle  steht

data Semester =
     Semester { enr :: ENr
	      , unr :: UNr -- jedes Semester ist einer Schule zugeordnet
	    , name :: Name
	       , von :: Time
	       , bis :: Time
	       -- | nicht in DB, aber bei Zugriff ausgerechnet
	       , status :: TimeStatus
	    }
	deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc] [''Semester])

-- local variables:
-- mode: haskell
-- end
