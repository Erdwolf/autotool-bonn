module Service.VerifyTaskConfig (
    verify_task_config
) where

import Types.Signed
import Types.Config
import Types.TT

verify_task_config :: TT Task -> TT Config -> IO (TT (Signed Config))
verify_task_config (TT task) (TT config) = undefined
