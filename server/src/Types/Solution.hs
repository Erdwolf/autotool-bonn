{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module Types.Solution (
    Solution (..)
) where

import Data.Autolib.Transport

-- a solution string
newtype Solution = SString String
    deriving (ToTransport, Eq, Read, Show)

-- $(derives [makeToTransport] [''Solution])
