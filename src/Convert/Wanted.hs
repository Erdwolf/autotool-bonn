-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell #-}

module Convert.Wanted where

import Autolib.Reader
import Autolib.ToDoc
import Data.Typeable

import qualified NFA.Property
import qualified Exp.Property

import Autolib.NFA ( NFAC )

data NFAC c Int => 
       Wanted c = NFA [ NFA.Property.Property c ]
	        | Exp  [ Exp.Property.Property c ]
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Wanted])
