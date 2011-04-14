-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-} 

module Control.Stud_Grp.Typ where

import Control.Types

import Autolib.Reader
import Autolib.ToDoc

import Data.Typeable

-- | das sollte exactly das sein, was auch in DB-tabelle  steht

data Stud_Grp  =
     Stud_Grp { snr :: SNr
	     , gnr :: GNr
	     }
     deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc] [''Stud_Grp])



