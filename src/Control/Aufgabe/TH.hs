{-# OPTIONS -fallow-overlapping-instances -fth #-}

module Control.Aufgabe.TH where

import Control.Aufgabe.Type
import Control.TH
import Network.XmlRpc.THDeriveXmlRpcType

$(asXmlRpcStruct ''Aufgabe)
