{-# OPTIONS -fallow-overlapping-instances -fth #-}

module Control.Student.TH where

import Control.Student.Type
import Control.TH
import Network.XmlRpc.Server
import Network.XmlRpc.THDeriveXmlRpcType

$(asXmlRpcStruct ''Student)
