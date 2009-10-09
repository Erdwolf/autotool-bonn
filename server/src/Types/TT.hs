{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.TT (
    TT (..)
) where

import Data.Autolib.Transport
import Data.Autolib.Transport.HaXR
import Network.XmlRpc.Internals
import Control.Monad

newtype TT a = TT { unTT :: a } deriving ToTransport

instance ToTransport a => XmlRpcType (TT a) where
    toValue = encode . toTransport . unTT
    fromValue = errorToErr . fmap TT . (fromTransport <=< decode)
    getType _ = TUnknown

errorToErr :: Monad m => Error a -> Err m a
errorToErr (Error  e) = fail e
errorToErr (Result x) = return x
