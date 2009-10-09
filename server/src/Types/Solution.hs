{-# LANGUAGE TemplateHaskell #-}

module Types.Solution (
    Solution (..)
) where

import Data.Autolib.Transport

data Solution = SString String

$(derives [makeToTransport] [''Solution])
