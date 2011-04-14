{-# LANGUAGE TemplateHaskell #-}

module Types.ScoringOrder (
    ScoringOrder (..),
) where

import Data.Autolib.Transport

data ScoringOrder = None | Increasing | Decreasing
    deriving (Eq, Read, Show)

$(derives [makeToTransport] [''ScoringOrder])
