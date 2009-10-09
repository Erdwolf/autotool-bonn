module Service.VerifyTaskConfig (
    verify_task_config
) where

import Util.Task
import Util.Sign
import Util.Description

import Types.Basic
import Types.Signed
import Types.Config
import Types.Description
import Types.TT

import Inter.Types
import Autolib.Reporter
import Autolib.Reader

import Control.Monad.Error

verify_task_config
    :: TT Task -> TT Config
    -> IO (TT (Either Description (Signed (Task, Config))))
verify_task_config (TT task) (TT (CString config)) = fmap TT . runErrorT $ do
    Make _ _ _ verifyConf _ <- lookupTaskM task
    config' <- either (fail . show) return $ parse reader "<config>" config
    let report = verifyConf config'
    case result report of
        Nothing -> throwError (fromOutput (kommentar report))
        _       -> return ()
    return (sign (task, CString config))
