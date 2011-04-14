{-# LANGUAGE MultiParamTypeClasses #-}

module Peano.Make ( make ) where

import qualified Peano.Data
import qualified Peano.Machine

import Inter.Types
import Autolib.Set

import qualified Machine.Numerical.Config as C
import qualified Machine.Numerical.Make as M
import qualified Machine.Numerical.Inter as I
import qualified Autolib.Reporter.Checker as R

instance C.Check () Peano.Data.Exp where

make :: Make
make = M.make  $ C.Config
     { C.name = "Peano"
     , C.conditions = []
     , C.arity = 2
     , C.op = read "x1 * x2" 
     , C.num_args = 10
     , C.max_arg = 20
     , C.cut = 1000
     , C.checks = [ ] :: [ () ]
     , C.start = Peano.Data.example
     }
