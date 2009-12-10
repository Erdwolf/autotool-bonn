module Fun.Make where

import Inter.Types

import Fun.Type
import Fun.Check
import Fun.Machine
import Fun.Examples

import qualified Machine.Numerical.Config as C
import qualified Machine.Numerical.Make as M
import qualified Machine.Numerical.Inter as I

import qualified Autolib.Reporter.Checker as R

import Autolib.Set

make :: Make
make = M.make $ C.Config
       { C.name = "Fun"
       , C.conditions = [ ]
       , C.arity = 2
       , C.op = read "x1 + x2" 
       , C.num_args = 10
       , C.max_arg = 20
       , C.cut = 1000
       , C.checks = [ Builtins [ Plus, Minus ]  ]
       , C.start = plus :: Fun
       }
