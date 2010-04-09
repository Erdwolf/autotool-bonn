{-# LANGUAGE TemplateHaskell #-}

module Types.TaskDescription (
    TaskDescription (..),
    module Types.Config,
    module Types.Documented,
    module Types.ScoringOrder
) where

import Types.Config
import Types.Documented
import Types.ScoringOrder

import Data.Autolib.Transport

data TaskDescription = TaskDescription {
    task_sample_config :: Documented Config,
    task_scoring_order :: ScoringOrder
} deriving (Read, Show)

$(derives [makeToTransport] [''TaskDescription])
