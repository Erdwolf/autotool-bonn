{-# OPTIONS -fallow-overlapping-instances -fth #-}

module Control.Vorlesung.TH where

import Control.Vorlesung.Type
import Control.TH
import Network.XmlRpc.THDeriveXmlRpcType

$(asXmlRpcStruct ''Vorlesung)
