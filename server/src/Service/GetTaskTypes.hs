module Service.GetTaskTypes (
    get_task_types
) where

import Util.Task

import Types.TT
import Types.TaskTree

get_task_types :: IO (TT [TaskTree])
get_task_types = return $ TT ttmakers
