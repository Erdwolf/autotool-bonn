{-# LANGUAGE TemplateHaskell #-}

module Types.Signed (
    Signed (..)
) where

import Types.Basic

import Data.Autolib.Transport

data Signed a = Signed { contents :: a, signature :: Signature }

$(derives [makeToTransport] [''Signed])
