module CNF.Eval where

import CNF.Form

import OBDD (OBDD)
import qualified OBDD.Make as M
import qualified OBDD.Operation as O

class Eval a where eval :: a -> OBDD Variable

instance Eval CNF where
    eval ( CNF clauses ) = O.and $ map eval clauses
instance Eval Clause where
    eval ( Clause literals ) = O.or $ map eval literals
instance Eval Literal where
    eval l = M.unit (variable l) (parity l) 


