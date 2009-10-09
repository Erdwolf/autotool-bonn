{-# LANGUAGE TemplateHaskell #-}

module Types.Version (
    Version (..)
) where

import Data.Autolib.Transport

data Version = Version {
    major :: Int,
    minor :: Int,
    micro :: Int
}

$(derives [makeToTransport] [''Version])
