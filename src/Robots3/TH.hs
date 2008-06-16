{-# OPTIONS -fglasgow-exts -fth -fallow-overlapping-instances #-}

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

instance XmlRpcType (Folge Zug) where
    getType f = TArray
    toValue ( Folge xs ) = ValueArray $ map toValue xs
    fromValue ( ValueArray vs ) = fmap Folge $ mapM fromValue vs

instance XmlRpcType Richtung where
    getType k = TString
    toValue k = ValueString $ "Richtung." ++ show k
    fromValue (ValueString s) = do
        let pre = "Richtung."
        guard $ isPrefixOf pre s
        return $ read $ drop ( length pre ) s

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

