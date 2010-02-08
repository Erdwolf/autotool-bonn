module Service.GetTaskDescription (
    get_task_description
) where

import Util.Task
import Util.Description

import Types.Basic
import Types.TaskDescription
import Types.TT

import Inter.Types

import qualified Autolib.ToDoc as AT

get_task_description :: TT Task -> IO (TT TaskDescription)
get_task_description (TT name) = TT `fmap` do
    m@(Make _ _ _ _ conf) <- lookupTaskM name
    doc <- help conf
    let sample = Documented
            (CString (AT.render . AT.toDoc $ conf))
            doc
    return $ TaskDescription { task_sample_config = sample,
                               task_scoring_order = taskScoringOrder m }
