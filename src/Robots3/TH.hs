{-# OPTIONS -fglasgow-exts -fth -fallow-overlapping-instances -fallow-incoherent-instances #-}

module Robots3.TH where

import Network.XmlRpc.Internals
import Network.XmlRpc.THDeriveXmlRpcType
import Robots3.Data
import Robots3.Config

import Inter.Types () -- get some default instances

import Data.List ( isPrefixOf )
import Control.Monad ( guard )

$(asXmlRpcStruct ''Position)
$(asXmlRpcStruct ''Robot)
$(asXmlRpcStruct ''Zug)


instance XmlRpcType Config where
    getType k = TStruct
    toValue k = ValueStruct
        [ ("robots", toValue $ robots k )
	, ("goals", toValue $ goals k )
	]
    fromValue ( ValueStruct vs ) = do
	rs <- getField "robots" vs
	gs <- getField "goals" vs
	rsv <- fromValue rs
	gsv <- fromValue gs
	return $ make rsv gsv
