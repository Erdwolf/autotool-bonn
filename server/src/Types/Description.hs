{-# LANGUAGE TemplateHaskell #-}

module Types.Description (
    Description (..)
) where

import Data.Autolib.Transport

data Description = DString String

$(derives [makeToTransport] [''Description])

