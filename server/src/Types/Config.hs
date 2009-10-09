{-# LANGUAGE TemplateHaskell #-}

module Types.Config (
    Config (..)
) where

import Data.Autolib.Transport

data Config = Config String

$(derives [makeToTransport] [''Config])
