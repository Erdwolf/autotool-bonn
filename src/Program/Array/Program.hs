{-# LANGUAGE DeriveDataTypeable #-}
module Program.Array.Program where

import Program.Array.Statement

import Autolib.Reader
import Autolib.ToDoc
import Autolib.TES.Identifier
import Autolib.Size

import Data.Typeable

data Program = Program [ Statement ]
    deriving ( Typeable )

s :: Program
s = Program [ s0 ]

d :: Program
d = Program [ d0, d1, d2 ]

instance ToDoc Program where
    toDoc ( Program ss ) = vcat $ map toDoc ss

instance Reader Program where
    reader = do
        ss <- many reader
	return $ Program ss

instance Size Program where
    size ( Program ss ) = sum $ map size ss
