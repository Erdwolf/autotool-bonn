{-# OPTIONS -fglasgow-exts #-}

module Control.Semester.Typ where

import Control.Types 

import Autolib.Reader
import Autolib.ToDoc

import Data.Typeable
import Text.XML.HaXml.Haskell2Xml hiding ( Name )

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

{-! for Semester derive: Reader, ToDoc, Haskell2Xml !-}

-- local variables:
-- mode: haskell
-- end
