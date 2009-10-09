module Service.GradeTaskSolution (
    grade_task_solution
) where

import Types.Basic
import Types.TT

grade_task_solution
    :: TT Task -> TT (Signed Instance) -> TT Solution
    -> TT (Documented (Pair Bool Double))
grade_task_solution = undefined
