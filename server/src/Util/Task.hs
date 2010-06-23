-- task type related utility functions

module Util.Task (
    ttmakers,
    lookupTask,
    lookupTaskM,
    taskScoringOrder
) where

import Types.TaskTree
import qualified Types.ScoringOrder as SO

import Inter.Collector
import Inter.Types

import qualified Data.Tree as T
import Data.Maybe

ttmakers :: [TaskTree]
ttmakers = mkTaskForest (T.subForest tmakers) where
    mkTaskForest = concat . map mkTaskTree
    mkTaskTree (T.Node (Left label) sub) = [Category label (mkTaskForest sub)]
    mkTaskTree (T.Node (Right task) [])  = [Task (show task) | validTask (show task)]
    mkTaskTree _ = error "malformed task tree in Inter.Collector"
    validTask x = not $ x `elem` ["Blank-Direct", "Upload-Direct"]

lookupTask :: String -> Maybe Make
lookupTask name = listToMaybe
    [m | m@(Make _ name' _ _ _) <- makers, name == name']

lookupTaskM :: Monad m => String -> m Make
lookupTaskM = maybe (fail "invalid task type") return . lookupTask

taskScoringOrder :: Make -> SO.ScoringOrder
taskScoringOrder = convertSO . scoringOrder where
    convertSO Increasing = SO.Increasing
    convertSO Decreasing = SO.Decreasing
    convertSO None = SO.None
