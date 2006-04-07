{-# OPTIONS -fallow-overlapping-instances -fallow-incoherent-instances #-}

module RAM.Make where

import Inter.Types

import RAM.Type
import RAM.Check
import RAM.Machine
import RAM.Property
import RAM.Example

import qualified Machine.Numerical.Config as C
import qualified Machine.Numerical.Make as M
import qualified Machine.Numerical.Inter as I

import qualified Autolib.Reporter.Checker as R

make :: Make
make = M.make $ C.Config
       { C.name = "RAM"
       , C.conditions = [ ]
       , C.arity = 2
       , C.op = read "x1 + x2"
       , C.num_args = 10
       , C.max_arg = 20
       , C.cut = 1000
       , C.checks = RAM.Property.example
       , C.start = RAM.Example.student
       }
