{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module Types.Config (
    Config (..)
) where

import Data.Autolib.Transport

newtype Config = CString String
    deriving (ToTransport, Eq, Read, Show)

-- $(derives [makeToTransport] [''Config])
