{-# LANGUAGE UndecidableInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Util.Sign (
    sign,
    verify,
    verifyM
) where

import Types.Signed as S
import Types.Basic
import Types.Config as C
import Types.Instance as I

import Util.Hash

class Sign a where
    sign_ :: a -> Signature

sign :: Sign a => a -> Signed a
sign a = Signed { S.contents = a, S.signature = sign_ a }

verify :: Sign a => Signed a -> Maybe a
verify (Signed { S.contents = a, S.signature = s })
    | sign_ a == s = Just a
    | otherwise    = Nothing

verifyM :: (Monad m, Sign a) => Signed a -> m a
verifyM = maybe (fail "invalid signature") return . verify

instance Hash a => Sign a where
    sign_ a = hash (secret, a)

secret :: String
secret = "secret"

instance Hash Config where
    hash (CString s) = hash ("CString", s)

instance Hash Instance where
    hash (Instance a b) = hash ("Instance", (a, b))
