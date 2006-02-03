{-# OPTIONS -fallow-overlapping-instances -fth #-}

module Control.TH where

import Inter.TH
import Inter.Crypt
import Control.Types
import Network.XmlRpc.Internals

-- Strings

instance XmlRpcType Email where
    toValue ( Email s ) = toValue s
    fromValue v = do s <- fromValue v ; return $ Email s
    getType _ = TString

instance XmlRpcType Name where
    toValue ( Name s ) = toValue s
    fromValue v = do s <- fromValue v ; return $ Name s
    getType _ = TString

$(Inter.TH.helper ''Crypt)

-- Show

instance XmlRpcType MNr where
    toValue ( MNr s ) = toValue $ show s
    fromValue v = do s <- fromValue v ; return $ MNr $ read s
    getType _ = TInt

-- Ints

instance XmlRpcType ANr where
    toValue ( ANr s ) = toValue s
    fromValue v = do s <- fromValue v ; return $ ANr s
    getType _ = TInt

instance XmlRpcType GNr where
    toValue ( GNr s ) = toValue s
    fromValue v = do s <- fromValue v ; return $ GNr s
    getType _ = TInt

instance XmlRpcType VNr where
    toValue ( VNr s ) = toValue s
    fromValue v = do s <- fromValue v ; return $ VNr s
    getType _ = TInt

instance XmlRpcType SNr where
    toValue ( SNr s ) = toValue s
    fromValue v = do s <- fromValue v ; return $ SNr s
    getType _ = TInt


