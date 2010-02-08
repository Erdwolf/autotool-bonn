{-# LANGUAGE TemplateHaskell #-}

module Types.ScoringOrder (
    ScoringOrder (..),
) where

import Data.Autolib.Transport
import Inter.Types

$(derives [makeToTransport] [''ScoringOrder])
