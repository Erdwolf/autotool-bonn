-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell #-}

module Control.Stud_Aufg.Typ where

import Control.Types

import Autolib.Reader
import Autolib.ToDoc

import Data.Typeable

-- | das sollte exactly das sein, was auch in DB-tabelle  steht

data Stud_Aufg  =
     Stud_Aufg { snr :: SNr
	     , anr :: ANr
             , ok :: Oks
	     , no :: Nos
             -- ignored: size, scoretime
             , instant :: Maybe File
	     , result :: Maybe Wert
	     , input :: Maybe File
	     , report :: Maybe File
	     }
     deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc] [''Stud_Aufg])



