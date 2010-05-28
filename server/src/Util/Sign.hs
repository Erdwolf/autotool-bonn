{-# LANGUAGE UndecidableInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- signing of data

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
import Data.Bits

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
    sign_ a = hmac secret (hash a)

-- XXX: this does not belong here.
secret :: Digest
secret = hash "secret"

-- cf. http://en.wikipedia.org/wiki/HMAC
hmac :: Digest -> String -> Digest
hmac secret = let
    key1 = map (toEnum . (xor 0x5c) . fromEnum) secret
    key2 = map (toEnum . (xor 0x36) . fromEnum) secret
  in
    \a -> hash (key1 ++ hash (key2 ++ a))

instance Hash Config where
    hash (CString s) = hash ("CString", s)

instance Hash Instance where
    hash (Instance a b) = hash ("Instance", (a, b))
