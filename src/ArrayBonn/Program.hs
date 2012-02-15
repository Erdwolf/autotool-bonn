{-# LANGUAGE DeriveDataTypeable #-}

module ArrayBonn.Program where

import Autolib.Reader
import Autolib.ToDoc
import Autolib.TES.Identifier
import Autolib.Size

import Data.Typeable

data Program st = Program [ st ]
    deriving ( Typeable )

plength ( Program sts ) = length sts

instance ToDoc st => ToDoc ( Program st ) where
    toDoc ( Program ss ) = vcat $ map toDoc ss

instance Reader st => Reader ( Program st ) where
    reader = do
        ss <- many reader
	return $ Program ss

instance Size st => Size ( Program st ) where
    size ( Program ss ) = sum $ map size ss
