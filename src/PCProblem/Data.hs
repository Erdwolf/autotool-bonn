{-# language TemplateHaskell #-}

module PCProblem.Data where

import Autolib.ToDoc
import Autolib.Reader

import Autolib.Set
import Autolib.Letters

import Data.Typeable
import Text.XML.HaXml.Haskell2Xml

import Network.XmlRpc.Internals
import Network.XmlRpc.THDeriveXmlRpcType

data PCP = PCP [(String, String)] deriving ( Typeable )

instance Letters PCP Char where
    letters ( PCP uvs ) = mkSet $ do (u, v) <- uvs ; u ++ v

{-! for PCP derive: ToDoc, Reader, Haskell2Xml !-}

data Pair = Pair { top :: String, bot :: String }

topair (x,y) = Pair x y
frompair p = ( top p, bot p )

instance XmlRpcType PCP where
    getType (PCP ps) = getType $ map topair ps
    toValue (PCP ps) = toValue $ map topair ps
    fromValue v = do
        ps <- fromValue v
	return $ PCP $ map frompair ps

$(asXmlRpcStruct ''Pair)

-- local variables:
-- mode: haskell
-- end:
