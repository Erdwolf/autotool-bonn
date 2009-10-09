{-# LANGUAGE TemplateHaskell #-}

module Types.Config (
    Config (..)
) where

import Data.Autolib.Transport

data Config = CString String

$(derives [makeToTransport] [''Config])
