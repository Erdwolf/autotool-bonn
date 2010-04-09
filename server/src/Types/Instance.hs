{-# LANGUAGE TemplateHaskell #-}

module Types.Instance (
    Instance (..)
) where

import Data.Autolib.Transport

data Instance = Instance { tag :: String, contents :: String }
    deriving (Eq, Read, Show)

$(derives [makeToTransport] [''Instance])
