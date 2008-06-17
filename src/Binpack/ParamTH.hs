{-# OPTIONS -fglasgow-exts -fth -fallow-overlapping-instances -fallow-incoherent-instances #-}

module Binpack.ParamTH where

import Network.XmlRpc.Internals
import Network.XmlRpc.THDeriveXmlRpcType
import Binpack.Param

import Inter.Types () -- get some default instances

$(asXmlRpcStruct ''Param)



