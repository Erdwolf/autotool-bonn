{-# OPTIONS -fallow-incoherent-instances #-}

module Turing.Make 

( computer
)

where

--  $Id$

import Inter.Types
import Autolib.Set

import Turing.Property
import Turing.Type
import qualified Turing.Example

import qualified Machine.Numerical.Config as C
import qualified Machine.Numerical.Make as M
import qualified Machine.Numerical.Inter as I
import qualified Autolib.Reporter.Checker as R


computer :: Make
computer = M.make $ C.Config
     { C.name = "Turing"
     , C.conditions = []
     , C.arity = 2
     , C.op = read "x1 * x2" 
     , C.num_args = 10
     , C.max_arg = 20
     , C.cut = 1000
     , C.checks = [ Sane ]
     , C.start = Turing.Example.student
     }






