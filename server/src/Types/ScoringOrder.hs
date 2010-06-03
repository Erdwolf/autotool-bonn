{-# LANGUAGE TemplateHaskell #-}

module Types.ScoringOrder (
    ScoringOrder (..),
) where

import Data.Autolib.Transport
import Inter.Types

-- type defined in Inter.Types now
$(derives [makeToTransport] [''ScoringOrder])
