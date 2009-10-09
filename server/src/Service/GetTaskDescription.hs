module Service.GetTaskDescription (
    get_task_description
) where

import Util.Task
import Util.Description

import Types.Basic
import Types.Documented
import Types.Config
import Types.TT

import Inter.Collector
import Inter.Types

import qualified Autolib.ToDoc as AT

get_task_description :: TT Task -> IO (TT (Documented Config))
get_task_description (TT name) = TT `fmap` do
    Make _ _ _ _ conf <-
        maybe (fail "invalid task type") return (lookupTask name)
    return $ Documented
            (Config (AT.render . AT.toDoc $ conf))
            (help conf)
