module Service.GradeTaskSolution (
    grade_task_solution
) where

import Util.Sign
import Util.Task
import Util.Description

import Types.Basic
import Types.Signed
import Types.Instance as I
import Types.Solution
import Types.Documented as D
import Types.TT

import Inter.Types
import Inter.Evaluate
import Autolib.Reader
import Autolib.Reporter
import Control.Types (is_okay, size)

import Control.Monad.Error
import Text.ParserCombinators.Parsec

grade_task_solution
    :: TT (Signed (Task, Instance)) -> TT Solution
    -> IO (TT (Either Description (Documented Double)))
grade_task_solution (TT sTaskInst) (TT (SString solution))
    = fmap TT . runErrorT $ do
        (task, inst) <- verifyM sTaskInst
        Make p _ maker0 _ _ <- lookupTaskM task
        inst' <- either (fail . show) return $ parse (parse_complete reader) "<instance>"
                 (I.contents inst)
        let assertTypes :: (conf -> Var p i b) -> (p, i) -> ()
            assertTypes _ _ = ()
            () = assertTypes maker0 (p, inst')
        let res = evaluate p inst' solution
        score <- case result res of
            Nothing -> throwReport res
            Just score -> return score
        when (not (is_okay score)) $ throwReport res
        doc <- liftIO $ fromReport res
        return $ Documented { D.contents = fromIntegral $ size score,
                              D.documentation = doc }

throwReport :: Reporter b -> ErrorT Description IO a
throwReport rep = liftIO (fromReport rep) >>= throwError