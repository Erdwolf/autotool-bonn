{-# OPTIONS -fglasgow-exts -fth -fallow-overlapping-instances -fallow-incoherent-instances #-}

module Binpack.InstanceTH where

import Network.XmlRpc.Internals
import Network.XmlRpc.THDeriveXmlRpcType
import Binpack.Instance 

import Inter.Types () -- get some default instances

$(asXmlRpcStruct ''Instance)



