-- -*- mode: haskell -*-

module Control.Student.Type where

import Control.Types
import Inter.Crypt

import Autolib.Reader
import Autolib.ToDoc

import Data.Typeable
import Text.XML.HaXml.Haskell2Xml hiding ( Name )

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

{-! for Student derive: Reader, ToDoc, Haskell2Xml !-}


