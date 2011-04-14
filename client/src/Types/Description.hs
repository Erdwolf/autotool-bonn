{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module Types.Description (
    Description (..)
) where

import Data.Autolib.Transport
import Util.Xml.Output

import qualified Control.Monad.Error as E

-- a description string
newtype Description = DString String
    deriving (ToTransport, Eq, Read, Show)

-- $(derives [makeToTransport] [''Description])

instance E.Error Description where
    noMsg      = E.strMsg "unknown error"
    strMsg msg = DString . stringToXmlString $ "error: " ++ msg
