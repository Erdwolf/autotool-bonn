module Service.VerifyTaskConfig (
    verify_task_config
) where

import Util.Task
import Util.Sign
import Util.Parse
import Util.Description
import Util.Timeout

import Types.Basic
import Types.Signed
import Types.Config
import Types.Description
import Types.TT

import Inter.Types
import Autolib.Reporter

import Control.Monad.Error

verify_task_config
    :: TT Task -> TT Config
    -> IO (TT (Either Description (Signed (Task, Config))))
verify_task_config (TT task) (TT (CString config))
    = withTimeout . fmap TT . runErrorT $ do
        Make _ _ _ verifyConf _ <- lookupTaskM task
        config' <- parseHelper "<config>" config
        let report = verifyConf config'
        case result report of
            Nothing -> liftIO (fromReport report) >>= throwError
            _       -> return ()
        return $ sign (task, CString config)
