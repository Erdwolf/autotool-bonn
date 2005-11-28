-- | template haskell helper

module Inter.TH where

import Network.XmlRpc.Internals
import Network.XmlRpc.THDeriveXmlRpcType
import Language.Haskell.TH

helper name = 
--    asXmlRpcStruct ( ( reify name ) >>= ( \ (TyConI dec) -> return dec ))
    asXmlRpcStruct name