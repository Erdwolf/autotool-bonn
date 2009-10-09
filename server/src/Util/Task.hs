module Util.Task (
    ttmakers,
    lookupTask,
    lookupTaskM
) where

import Types.TaskTree

import Inter.Collector
import Inter.Types

import qualified Data.Tree as T
import Data.Maybe

ttmakers :: [TaskTree]
ttmakers = mkTaskForest (T.subForest tmakers) where
    mkTaskForest = map mkTaskTree
    mkTaskTree (T.Node (Left label) sub) = Category label (mkTaskForest sub)
    mkTaskTree (T.Node (Right task) [])  = Task (show task)

lookupTask :: String -> Maybe Make
lookupTask name = listToMaybe
    [m | m@(Make _ name' _ _ _) <- makers, name == name']

lookupTaskM :: Monad m => String -> m Make
lookupTaskM = maybe (fail "invalid task type") return . lookupTask