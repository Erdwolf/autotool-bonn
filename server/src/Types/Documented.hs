{-# LANGUAGE TemplateHaskell #-}

module Types.Documented (
    Documented (..),
    module Types.Description
) where

import Types.Description

import Data.Autolib.Transport

-- something documented: a value together with its description
data Documented a = Documented { contents :: a, documentation :: Description }
    deriving (Eq, Read, Show)

$(derives [makeToTransport] [''Documented])
