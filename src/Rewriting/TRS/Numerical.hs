module Rewriting.TRS.Numerical

( make )

where

--  $Id$

import Inter.Types
import Autolib.Set
import Autolib.Size

import Rewriting.TRS.Machine
import Rewriting.TRS
import Rewriting.Check

import qualified Machine.Numerical.Config as C
import qualified Machine.Numerical.Make as M
import qualified Machine.Numerical.Inter as I
import qualified Autolib.Reporter.Checker as R

instance ( Symbol c, Symbol v ) => Size ( TRS v c ) where 
    size trs = sum $ do
        r <- regeln trs
        t <- [ lhs r, rhs r ]
        return $ size t

make :: Make
make = M.make $ C.Config
     { C.name = "TRS"
     , C.conditions = []
     , C.arity = 2
     , C.op = read "x1 * x2" 
     , C.num_args = 10
     , C.max_arg = 20
     , C.cut = 1000
     , C.checks = [ ] :: [ Check ]
     , C.start = Rewriting.TRS.example
     }






