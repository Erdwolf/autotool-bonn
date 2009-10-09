module Service.GetTaskInstance (
    get_task_instance
) where

import Util.Sign
import Util.Task
import Util.Description

import Types.Basic
import Types.Signed as S
import Types.Documented as D
import Types.Config
import Types.Instance as I
import Types.Solution
import Types.TT

import Inter.Types as IT
import Control.Types (VNr (..))
import Autolib.Reporter
import Autolib.Reader
import qualified Autolib.ToDoc as AT
import Challenger.Partial as CP

import Text.ParserCombinators.Parsec

get_task_instance
    :: TT (Signed (Task, Config)) -> TT Seed
    -> IO (TT (Signed (Task, Instance), Description, Documented Solution))
get_task_instance  (TT sconf) (TT seed) = fmap TT $ do
    (task, CString config) <- verifyM sconf
    Make _ _ maker0 _ _ <- lookupTaskM task
    let Right config' = parse reader "<config>" config
        maker = maker0 config'
    ri <- gen maker (VNr 0) Nothing seed
    i <- maybe (fail "internal error generating instance") return (result ri)
    let b = CP.initial (problem maker) i
    return ( sign (task,
                   Instance { I.tag = IT.tag maker,
                              I.contents = AT.showDoc . AT.toDoc $ i})
           , fromDoc $ CP.describe (problem maker) i
           , Documented { D.contents = SString . AT.render . AT.toDoc $ b,
                          D.documentation = help b }
           )
