-- -*- mode: haskell -*-

module Convert.Wanted where

import Autolib.Reader
import Autolib.ToDoc
import Data.Typeable
import Text.XML.HaXml.Haskell2Xml

import qualified NFA.Property
import qualified Exp.Property

import Autolib.NFA ( NFAC )

data NFAC c Int => 
       Wanted c = NFA [ NFA.Property.Property c ]
	        | Exp  [ Exp.Property.Property c ]
     deriving ( Typeable )

{-! for Wanted derive: Reader, ToDoc, Haskell2Xml !-}
