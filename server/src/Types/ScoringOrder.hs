{-# LANGUAGE TemplateHaskell #-}

module Types.ScoringOrder (
    ScoringOrder (..),
) where

import Data.Autolib.Transport

data ScoringOrder = Increasing | None | Decreasing

$(derives [makeToTransport] [''ScoringOrder])
