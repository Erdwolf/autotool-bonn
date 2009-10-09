{-# LANGUAGE TemplateHaskell #-}

module Types.Instance (
    Instance (..)
) where

import Data.Autolib.Transport

data Instance = Instance { tag :: String, contents :: String }

$(derives [makeToTransport] [''Instance])
