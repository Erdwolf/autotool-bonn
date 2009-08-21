-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell #-}

module Control.Student.Type where

import Control.Types
import Inter.Crypt

import Autolib.Reader
import Autolib.ToDoc

import Data.Typeable

data Student =
     Student { snr :: SNr
	     , unr :: UNr
	     , mnr :: MNr
	     , name :: Name
	     , vorname :: Name
             , email :: Email
	     , passwort :: Crypt
             , next_passwort :: Crypt
	     }
     deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc] [''Student])


