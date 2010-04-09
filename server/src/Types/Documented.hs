{-# LANGUAGE TemplateHaskell #-}

module Types.Documented (
    Documented (..),
    module Types.Description
) where

import Types.Description

import Data.Autolib.Transport

data Documented a = Documented { contents :: a, documentation :: Description }
    deriving (Eq, Read, Show)

$(derives [makeToTransport] [''Documented])
