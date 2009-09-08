-- -*- mode: haskell -*-

{-# LANGUAGE TemplateHaskell #-}
module Control.Schule.Typ where

import Control.Types ( UNr, Name )

import Autolib.Reader
import Autolib.ToDoc

import Data.Typeable
-- import Text.XML.HaXml.Haskell2Xml hiding ( Name )

-- | das sollte exactly das sein, was auch in DB-tabelle  steht

data Schule =
     Schule { unr :: UNr
	    , name :: Name
            , mail_suffix :: Name
                  -- ^ Studenten werden nur akzeptiert,
                  -- wenn email so endet
	    }
	deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Schule])
-- {-! for Schule derive: Reader, ToDoc, Haskell2Xml !-}
