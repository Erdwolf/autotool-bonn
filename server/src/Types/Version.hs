{-# LANGUAGE TemplateHaskell #-}

module Types.Version (
    Version (..)
) where

import Data.Autolib.Transport

-- version record
data Version = Version {
    major :: Int,
    minor :: Int,
    micro :: Int
} deriving (Eq, Read, Show)

$(derives [makeToTransport] [''Version])
