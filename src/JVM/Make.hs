module JVM.Make 

( make )

where

--  $Id$

import Inter.Types
import Autolib.Set

import JVM.Check
import JVM.Type
import qualified JVM.Example

import qualified Machine.Numerical.Config as C
import qualified Machine.Numerical.Make as M
import qualified Machine.Numerical.Inter as I
import qualified Autolib.Reporter.Checker as R

make :: Make
make = M.make $ C.Config
     { C.name = "JVM"
     , C.arity = 2
     , C.op = read "x1 * x2" 
     , C.num_args = 10
     , C.max_arg = 20
     , C.cut = 1000
     , C.checks = [ Builtins $ mkSet [ Add, Sub, Mul ]  ]
     , C.start = JVM.Example.student
     }






