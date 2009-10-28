{-# LANGUAGE TemplateHaskell #-}

module Types.Description (
    Description (..)
) where

import Data.Autolib.Transport
import Autolib.Output
import Util.Xml.Output

import qualified Control.Monad.Error as E

data Description = DString String

$(derives [makeToTransport] [''Description])

instance E.Error Description where
    noMsg      = E.strMsg "unknown error"
    strMsg msg = DString . outputToXmlString . Text $ "error: " ++ msg
