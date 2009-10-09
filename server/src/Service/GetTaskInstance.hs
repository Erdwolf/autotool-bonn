module Service.GetTaskInstance (
    get_task_instance
) where

import Types.Basic
import Types.Task
import Types.Signed
import Types.Documented
import Types.TT

get_task_instance
    :: TT Task -> TT (Signed Config) -> TT Seed
    -> IO (TT (Pair (Documented (Signed Instance)) (Documented Solution)))
get_task_instance (TT task) (TT sconf) (TT seed) = undefined
