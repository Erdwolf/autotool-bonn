-- -*- mode: haskell -*-

{-# LANGUAGE TemplateHaskell #-}
module Control.Stud_Grp.Typ where

import Control.Types

import Autolib.Reader
import Autolib.ToDoc

import Data.Typeable
-- import Text.XML.HaXml.Haskell2Xml hiding ( Name )

-- | das sollte exactly das sein, was auch in DB-tabelle  steht

data Stud_Grp  =
     Stud_Grp { snr :: SNr
	     , gnr :: GNr
	     }
     deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc] [''Stud_Grp])
-- {-! for Stud_Grp derive: Reader, ToDoc, Haskell2Xml !-}



